library(psrccensus)
library(psrctrends)
library(psrcplot)
library(psrcslides)

library(tidyverse)
library(tidycensus)
library(sf)
library(officer)
library(data.table)

install_psrc_fonts()
psrc_pres = read_pptx(system.file('extdata', 'psrc-trends-template.pptx', package='psrcslides'))
                             
# Cover Slide -------------------------------------------------------------
psrc_pres <- add_cover_slide(p = psrc_pres,
                             p.title = "PSRC Trends",
                             p.mtg = "Growth Managment Policy Board",
                             p.date = "September 2022")

# Introduction Slide ------------------------------------------------------
psrc_pres <- add_full_text_slide(p=psrc_pres, 
                                 p.title="Presentation Outline",
                                 p.caption="Topics covered today include:",
                                 p.text=paste0("Population Trends\n",
                                               "Housing Trends\n",
                                               "Job Trends\n",
                                               "Transportation Trends"))

# Population Transition --------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Population Trends",
                                  p.img=('X:/DSA/shiny-uploads/images/slide-uw-equity_0.png'))

# Regional Population Change ----------------------------------------------
ofm.pop <- get_ofm_intercensal_population() %>% 
  filter(Jurisdiction=="Region") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

pop.growth <- ofm.pop %>% filter(Year=="2022") %>% select(Delta) %>% pull()
pop.ovr.50k <- ofm.pop %>% filter(Delta>=50000) %>% select(Delta) %>% pull() %>% length()

pop.change <- create_column_chart(t=ofm.pop, w.x="Year", w.y="Delta", f="Variable", 
                               w.color="psrc_light", w.title="Population Change: 2013 to 2022",
                               est.type = "number") +
  labs(caption = paste0("Source: Office of Financial Management Intercensal Population Estimates")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")
  

pop.caption <- paste0("The region added ", 
                      prettyNum(pop.growth, big.mark = ","),
                      " people between 2021 and 2022. In ",
                      pop.ovr.50k, 
                      " out of the last 10 years the region has grown by more than 50,000 people.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Despite a Global Pandemic, the region continues to grow",
                                          p.caption = pop.caption,
                                          p.chart = pop.change)

# Population Growth Compared to Vision 2050 -------------------------------
observed.pop <- get_ofm_intercensal_population() %>% 
  filter(Jurisdiction=="Region" & Year>=2010) %>% 
  mutate(Variable="Observed") %>% 
  select(Year, Estimate, Variable)

observed.forecast.pop <- observed.pop %>% 
  filter(Year<=2018) %>% 
  mutate(Variable="Forecast") %>% 
  select(Year, Estimate, Variable)

forecast.pop <- get_elmer_table("Macroeconomic.pop_facts") %>% 
  filter(pop_group_dim_id==7 & data_year >= 2019) %>%
  rename(Year=data_year,Estimate=population) %>%
  mutate(Variable="Forecast") %>% 
  select(Year, Estimate, Variable)

pop.growth.vision <- bind_rows(list(observed.pop, observed.forecast.pop, forecast.pop)) %>% filter(Year<=2025) %>% mutate(Year=as.character(Year))
pop.vision.2022 <- pop.growth.vision %>% filter(Year=="2022" & Variable=="Forecast") %>% select(Estimate) %>% pull()
pop.observed.2022 <- pop.growth.vision %>% filter(Year=="2022" & Variable=="Observed") %>% select(Estimate) %>% pull()
pop.observed.2022.delta <- pop.observed.2022 - pop.vision.2022
pop.observed.2022.delta.share <- 1 - (pop.observed.2022.delta / pop.vision.2022)

pop.growth.vision.chart <- create.line.chart(t=pop.growth.vision, w.x='Year', w.y='Estimate', w.g='Variable', est.type="number", 
                                             w.title="Annual Population Forecast: 2010 to 2025",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025"),
                                             w.color = "DkPrLtPr") +
  labs(caption = paste0("Source: Office of Financial Management Intercensal Population Estimates & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

pop.vision.caption <- paste0("Between 2018 and 2022, population growth is in-line with VISION 2050. In VISION 2050 forecasts, the population in 2022 was forecasted to be ", 
                      prettyNum(round(pop.vision.2022,-3), big.mark = ","),
                      ". The observed population in 2022 was  ",
                      prettyNum(round(pop.observed.2022,-3), big.mark = ","), 
                      ", a difference of ",
                      prettyNum(round(pop.observed.2022.delta,-3), big.mark = ","),
                      " people.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "So far, so good",
                                          p.caption = pop.vision.caption,
                                          p.chart = pop.growth.vision.chart)

# Components of Population Change -----------------------------------------
ofm.pop.chg <- get_ofm_postcensal_pop_change()
tbl <- ofm.pop.chg %>% filter(Jurisdiction=="Region" & Year >= "2013")

migration.2022 <- tbl %>% filter(Year=="2022" & Variable=="Migration") %>% select(Estimate) %>% pull()
natural.2022 <- tbl %>% filter(Year=="2022" & Variable=="Natural Increase") %>% select(Estimate) %>% pull()
migration.share.2022 <- migration.2022/(migration.2022+natural.2022)

migration.ten <- tbl %>% filter(Variable=="Migration") %>% select(Estimate) %>% sum()
natural.ten <- tbl %>% filter(Variable=="Natural Increase") %>% select(Estimate) %>% sum()
migration.share.ten <- migration.ten/(migration.ten+natural.ten)

population.type.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Estimate", f="Variable", 
                                                    w.title="Annual Components of Population Change: 2013 to 2022",
                                                    est.type = "number",
                                                    w.pos="stack",
                                                    w.color = "OrPr") +
  labs(caption = paste0("Source: Office of Financial Management Intercensal Population Estimates")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

pop.component.caption <- paste0("Approximately ", 
                                prettyNum(round(migration.2022,-2), big.mark = ","),
                                " people migrated to the region between 2021 and 2022, ",
                                prettyNum(round(migration.share.2022*100,0), big.mark = ","),
                                "% of the total growth. In all,migration has accounted for ",
                                prettyNum(round(migration.share.ten*100,0), big.mark = ","),
                                "% of the total growth in the past 10 years")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Migration continues to drive population growth",
                                          p.caption = pop.component.caption,
                                          p.chart = population.type.change.chart)

# Population by Race for Region------------------------------------------------------
white.alone <- c("B03002_003")
people.of.color <- c("B03002_004","B03002_005","B03002_006","B03002_007","B03002_008","B03002_009","B03002_012")

pop.by.race <- get_acs_recs(geography = 'county',table.names = c('B03002'), years=c(seq(2010,2020,by=1)), acs.type = 'acs5')

tbl <- pop.by.race %>%
  filter(variable %in% c(people.of.color, white.alone)) %>%
  mutate(POC = case_when(
    variable %in% people.of.color ~ "Person of Color",
    variable %in% white.alone ~ "Non-Hispanic White")) %>%
  select(name, POC, year, estimate) %>%
  group_by(name, POC, year) %>%
  summarise(estimate=sum(estimate))

tot <- pop.by.race %>%
  filter(variable %in% c("B03002_001")) %>%
  select(name, year, estimate) %>%
  rename(total=estimate)

tbl <- left_join(tbl, tot, by=c("name","year")) %>%
  mutate(share=estimate/total, year=as.character(year))

poc.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="Region") %>% select(share) %>% pull()
poc.king.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="King County") %>% select(share) %>% pull()

pop.by.race.chart <- create.line.chart(t=tbl %>% filter(name=="Region" & POC=="Person of Color"), 
                                       w.x='year', w.y='share', w.g='POC', est.type="percent", 
                                       w.title="People of Color: 2010 to 2020",
                                       x.type= "Continuous",
                                       w.lwidth = 3,
                                       w.breaks = c("2010","2015","2020"),
                                       w.color = "LtPrDkPr") +
  labs(caption = paste0("Source: American Community Survey Data Table B03002")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5))

# Indigenous and Asian
tbl <- pop.by.race %>%
  filter(variable %in% c(people.of.color, white.alone)) %>%
  select(name, variable, label, year, estimate)

tot <- pop.by.race %>%
  filter(variable %in% c("B03002_001")) %>%
  select(name, year, estimate) %>%
  rename(total=estimate)

tbl <- left_join(tbl, tot, by=c("name","year")) %>%
  mutate(share=estimate/total, year=as.character(year))

indigenous.tot <- tbl %>% filter(year=="2020" & name=="Region" & variable=="B03002_005") %>% select(estimate) %>% pull()
indigenous.shr <- tbl %>% filter(year=="2020" & name=="Region" & variable=="B03002_005") %>% select(share) %>% pull()

asian.tot <- tbl %>% filter(year=="2020" & name=="Region" & variable=="B03002_006") %>% select(estimate) %>% pull()
asian.shr <- tbl %>% filter(year=="2020" & name=="Region" & variable=="B03002_006") %>% select(share) %>% pull()


pop.by.race.caption <- paste0("In 2020, People of Color accounted for approximately ",
                              prettyNum(round(poc.2020*100,0), big.mark = ","),
                              "% of the total population in the region")

pop.by.race.bullets <- paste0("In 2020 there were approximately ", 
                              prettyNum(round(indigenous.tot,-2), big.mark = ","),
                              " Indigenous people living in the Puget Sound region.\n",
                              "Approximately ", 
                              prettyNum(round(asian.shr*100,1), big.mark = ","),
                              "% of the total population in the region were of Asian descent.\n",
                              "People of Color accounted for approximately ", 
                              prettyNum(round(poc.king.2020*100,0), big.mark = ","),
                              "% of the total population in King County, the largest share of any county.")
  
psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="The region continues to diversify", 
                                         p.caption=pop.by.race.caption, 
                                         p.bullet=pop.by.race.bullets, 
                                         p.chart=pop.by.race.chart)

# MPO Comparison Data ----------------------------------------------------------------
census.yr <- 2020
mpo <- read_csv("X:/DSA/shiny-uploads/data/regional-councils-counties.csv") %>% 
  mutate(COUNTY_FIPS=str_pad(COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
  mutate(STATE_FIPS=str_pad(STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
  mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS))

states <- mpo %>% select(STATE_FIPS) %>% distinct() %>% pull()
counties <- mpo %>% select(GEOID) %>% distinct() %>% pull()

mpo_county_data <- NULL
for (st in states) {
  
  c <- mpo %>% filter(STATE_FIPS %in% st) %>% select(COUNTY_FIPS) %>% pull()
  
  pop <- get_acs(geography = "county", state=st, county=c, variables = c("B03002_001","B03002_003"), year = census.yr, survey = "acs5") %>% select(-moe)
  ms <- get_acs(geography = "county", state=st, county=c, variables = c("B08006_001","B08006_003"), year = census.yr, survey = "acs5") %>% select(-moe)
  d <- bind_rows(pop,ms)
  
  ifelse(is.null(mpo_county_data), mpo_county_data <- d, mpo_county_data <- bind_rows(mpo_county_data,d))
  
  rm(d, c, pop, ms)
}

mpo_county_data <- mpo_county_data %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(People_of_Color=(B03002_001-B03002_003), NonSOV_Commute_Trips=(B08006_001-B08006_003)) %>%
  rename(Population=B03002_001, Non_Hispanic_White=B03002_003, Commute_Trips=B08006_001, SOV_Commute_Trips=B08006_003) %>%
  select(GEOID, Population, People_of_Color, Non_Hispanic_White, Commute_Trips, NonSOV_Commute_Trips, SOV_Commute_Trips)

mpo_county_data <- left_join(mpo, mpo_county_data, by="GEOID")  

fars_data <- NULL
for (y in seq(census.yr-4,census.yr,by=1)) {
  
  # Open Current Years FARS Accident Data
  all_files <- as.character(unzip(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), list = TRUE)$Name)
  
  c <- read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1])) %>%
    mutate(COUNTY_FIPS=str_pad(COUNTY, width=3, side=c("left"), pad="0")) %>%
    mutate(STATE_FIPS=str_pad(STATE, width=2, side=c("left"), pad="0")) %>%
    mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS)) %>%
    filter(GEOID %in% counties) %>%
    select(GEOID, FATALS) %>%
    mutate(FATAL_COLLISIONS = 1) %>%
    group_by(GEOID) %>%
    summarise(Fatalities=sum(FATALS), Fatal_Collisions =sum(FATAL_COLLISIONS)) %>%
    as_tibble
  
  ifelse(is.null(fars_data), fars_data <- c, fars_data <- bind_rows(fars_data,c))
  
  rm(c)
  
}

fars_data <- fars_data %>%
  group_by(GEOID) %>%
  summarise(Fatalities_5yr=sum(Fatalities), Fatal_Collisions_5yr =sum(Fatal_Collisions)) %>%
  as_tibble

mpo_county_data <- left_join(mpo_county_data, fars_data, by="GEOID") 
rm(mpo, fars_data)

mpo_data <- mpo_county_data %>%
  select(-MSA_FIPS, -MSA_NAME, -COUNTY_FIPS, -COUNTY_NAME, -STATE_FIPS, -STATE_NAME, -GEOID) %>%
  group_by(MPO_AREA, MPO_FIPS, MPO_NAME) %>%
  summarise(across(.fns = sum)) %>%
  mutate(People_of_Color_Share = People_of_Color/Population) %>%
  mutate(NonSOV_Share = NonSOV_Commute_Trips/Commute_Trips) %>%
  mutate(Fatal_Collision_Rate = ((Fatal_Collisions_5yr/5)/Population)*100000)

# Population by Metro -----------------------------------------------------
metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(People_of_Color_Share)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

pop.by.race.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="People_of_Color_Share", 
                                          f="plot_id", w.color = "psrc_light",
                                          w.title="People of Color by MPO: 2020") +
  labs(caption = paste0("Source: 2016-2020 American Community Survey 5yr Data Table B03002")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "The region is less diverse than many Metro Areas",
                                  p.chart = pop.by.race.mpo.chart)

# Regional Geography Population Growth ------------------------------------
tbl <- get_ofm_intercensal_population() %>% 
  filter(Filter%in%c(2,4)) %>%
  filter(!(str_detect(Jurisdiction, "Region"))) %>%
  drop_na() %>%
  select(-Filter,-Jurisdiction,-Variable) %>%
  group_by(Year,regional_geography) %>%
  summarize_all(sum) %>%
  as_tibble()

cities <- tbl %>%
  filter(regional_geography=="Cities & Towns") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

core <- tbl %>%
  filter(regional_geography=="Core Cities") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

metro <- tbl %>%
  filter(regional_geography=="Metropolitan Cities") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

hct <- tbl %>%
  filter(regional_geography=="High Capacity Transit Community") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

unincorporated <- tbl %>%
  filter(regional_geography=="Unincorporated") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

ofm.pop.rgeo <- bind_rows(list(metro,core,hct,cities,unincorporated))
rm(tbl,metro,core,hct,cities,unincorporated)

total.pop.growth.2022 <- migration.2022+natural.2022
metro.pop.growth.2022 <- ofm.pop.rgeo %>% filter(Year=="2022" & `Regional Geography`=="Metropolitan Cities") %>% select(Delta) %>% pull()
core.pop.growth.2022 <- ofm.pop.rgeo %>% filter(Year=="2022" & `Regional Geography`=="Core Cities") %>% select(Delta) %>% pull()
hct.pop.growth.2022 <- ofm.pop.rgeo %>% filter(Year=="2022" & `Regional Geography`=="High Capacity Transit Community") %>% select(Delta) %>% pull()

metro.pop.growth.share.2022 <- metro.pop.growth.2022 / total.pop.growth.2022
top3.pop.growth.share.2022 <- (metro.pop.growth.2022+core.pop.growth.2022+hct.pop.growth.2022) / total.pop.growth.2022

seattle <- get_ofm_intercensal_population() %>% filter(Jurisdiction=="Seattle") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) 

seattle.pop.growth.2022 <- seattle %>% filter(Year==2022) %>% select(Delta) %>% pull()
seattle.pop.growth.share.2022 <- seattle.pop.growth.2022 / total.pop.growth.2022

metro.pop.growth.ten <- ofm.pop.rgeo %>% filter(Year>="2013" & `Regional Geography`=="Metropolitan Cities") %>% select(Delta) %>% sum()
all.pop.growth.ten <- ofm.pop.rgeo %>% filter(Year>="2013") %>% select(Delta) %>% sum()

metro.pop.growth.share.ten <- metro.pop.growth.ten/all.pop.growth.ten

cities <- get_ofm_intercensal_population() %>% 
  filter(Filter==4) %>%
  filter(Year==2013 | Year==2022) %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year==2022) %>%
  arrange(desc(Delta)) %>%
  mutate(Rank = order(Delta, decreasing = TRUE))

top.five.cities <- cities %>% filter(Rank<=5) %>% select(Jurisdiction) %>% pull()
top.five.cities.growth <- cities %>% filter(Rank<=5) %>% select(Delta) %>% pull() %>% sum()

# Growth in 2022
tbl <- ofm.pop.rgeo %>% filter(Year=="2022") %>% mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT"))
pop.rgeo.chart.22 <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", 
                                          w.title = "Population Growth by Regional Geography: 2022", 
                                          est.type = "number") +
  labs(caption = paste0("Source: Office of Financial Management Intercensal Population Estimates")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

pop.by.rgeo.caption <- paste0("Between 2021 and 2022, ",
                              prettyNum(round(metro.pop.growth.share.2022*100,0), big.mark = ","),
                              "% of the total population growth in the region was in Metropolitan Cities.")

pop.by.rgeo.bullets <- paste0("The City of Seattle added ", 
                              prettyNum(round(seattle.pop.growth.2022,-2), big.mark = ","),
                              " people in 2022, about ",
                              prettyNum(round(seattle.pop.growth.share.2022*100,0), big.mark = ","),
                              "% of the total regional population growth last year.\n",
                              "Approximately ", 
                              prettyNum(round(metro.pop.growth.ten,-2), big.mark = ","),
                              " people moved into Metropolitan Cities in the last ten years, ",
                              prettyNum(round(metro.pop.growth.share.ten*100,0), big.mark = ","),
                              "% of the total regional population growth.\n",
                              "The five fastest growing cities in the past ten years were: ", 
                              paste(top.five.cities,collapse = ", "),
                              ". These five citites added more than ",
                              prettyNum(round(top.five.cities.growth,-2), big.mark = ","),
                              " people in the last ten years.")

psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="Big Cities are not dying", 
                                         p.caption=pop.by.rgeo.caption, 
                                         p.bullet=pop.by.rgeo.bullets, 
                                         p.chart=pop.rgeo.chart.22)

# Population Growth near HCT ----------------------------------------------
wgs84 <- 4326
spn <- 2285 
hct.buffer <- 0.5

# Blocks with HCT Stops
hct.stops <- st_read('X:/DSA/shiny-uploads/data/hct_stops_2050.shp') %>% 
  st_transform(spn) %>%
  st_buffer(dist=hct.buffer*5280) %>%
  st_union() %>%
  st_sf() %>%
  mutate(HCT_Buffer = "Yes")

psrc.blocks <- st_read('X:/DSA/shiny-uploads/data/block2010.shp')%>% 
  st_transform(spn) %>%
  select(GEOID10)

psrc.hct.blocks <- st_intersection(psrc.blocks, hct.stops)

blocks <- psrc.blocks %>% st_drop_geometry()
hct.blocks <- psrc.hct.blocks %>% st_drop_geometry()

final.blocks <- left_join(blocks, hct.blocks, by=c("GEOID10")) %>% mutate(HCT_Buffer = replace_na(HCT_Buffer,"No"))

# Load Population and Housing Data by Census Block
block.pop <- get_elmer_table("ofm.estimate_facts") %>% filter(publication_dim_id == 3)
block.dim <- get_elmer_table("census.geography_dim") %>% filter(geography_type %in% c("Block")) %>% select(geography_dim_id, block_geoid)

ofm.block.data <- left_join(block.pop, block.dim, by=c("geography_dim_id")) %>%
  mutate(total_population = household_population + group_quarters_population) %>%
  select(block_geoid, housing_units, household_population, group_quarters_population, total_population,estimate_year)

ofm.block.data <- left_join(ofm.block.data, final.blocks, by=c("block_geoid"="GEOID10"))

population.near.hct <- ofm.block.data %>%
  select(HCT_Buffer, estimate_year, total_population) %>%
  group_by(HCT_Buffer, estimate_year) %>%
  summarise(Population = sum(total_population)) %>%
  as_tibble() %>%
  pivot_wider(id_cols=estimate_year, names_from=HCT_Buffer, values_from=Population) %>%
  rename(Year=estimate_year, NonHCT=No, HCT=Yes) %>%
  mutate(Total=NonHCT+HCT) %>%
  mutate(HCT_Share_Total = HCT/Total) %>%
  mutate(Delta_Total = round(Total - lag(Total),-2)) %>%
  mutate(Delta_HCT = round(HCT - lag(HCT),-2)) %>%
  mutate(HCT_Share_Growth = Delta_HCT/Delta_Total) %>%
  filter(Year>=2013) %>%
  mutate(variable="Population Growth") %>%
  mutate(Year = as.character(Year))

hct.growth.since.2018 <- population.near.hct %>% filter(Year >= "2018") %>% select(Delta_HCT) %>% pull() %>% sum()
tot.growth.since.2018 <- population.near.hct %>% filter(Year >= "2018") %>% select(Delta_Total) %>% pull() %>% sum()
hct.growth.share.since.2018 <- hct.growth.since.2018/tot.growth.since.2018

pop.growth.near.hct.chart <- create.line.chart(t=population.near.hct, w.x="Year", w.y="HCT_Share_Growth", w.g="variable",
                                               w.title = "Population Growth near Current and Future High Capacity Transit: 2013 to 2020", 
                                               x.type= "Continuous",
                                               est.type = "percent",
                                               w.lwidth = 3,
                                               w.breaks = c("2013","2014","2015","2016","2017","2018","2019","2020"),
                                               w.color = "BkPr") +
  labs(caption = paste0("Source: Office of Financial Management Small Area Estimates Program")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  scale_y_continuous(labels = scales::label_percent(), limits=c(0,1))

pop.growth.near.hct.caption <- paste0("Between 2018 and 2020, approximately ", 
                                      prettyNum(round(hct.growth.share.since.2018*100,0), big.mark = ","),
                                      "% of population growth has occurred within a half mile of current or planned ",
                                      "high capacity transit.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Over half of population growth is near High Capacity Transit",
                                          p.caption = pop.growth.near.hct.caption,
                                          p.chart = pop.growth.near.hct.chart)

# Housing Transition --------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Housing Trends",
                                  p.img=('X:/DSA/shiny-uploads/images/housing.jpg'))

# Regional Housing Production ---------------------------------------------
ofm.housing <- get_ofm_postcensal_housing()

# Annual Regional Housing Unit Change
tbl <- ofm.housing %>% 
  filter(Jurisdiction=="Region" & Variable == "Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

housing.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                                            w.color="OrGn",
                                            w.title="Annual Housing Unit Change: 2013 to 2022",
                                            w.interactive="no",
                                            est.type = "number") +
  labs(caption = paste0("Source: Office of Financial Management")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

region.housing.delta.2022 <- tbl %>% filter(Year=="2022") %>% select(Delta) %>% pull()

region.housing.delta.since.1992 <- ofm.housing %>% 
  filter(Jurisdiction=="Region" & Variable == "Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=1992) %>%
  mutate(Year = as.character(Year)) %>%
  arrange(desc(Delta)) %>%
  mutate(Rank = order(Delta, decreasing = TRUE))

region.housing.delta.2022.rank <- region.housing.delta.since.1992 %>% filter(Year=="2022") %>% select(Rank) %>% pull()
housing.since.1992 <- region.housing.delta.since.1992 %>% select(Delta) %>% pull() %>% length

housing.growth.caption <- paste0("Approximately ", 
                                 prettyNum(round(region.housing.delta.2022,-2), big.mark = ","),
                                 " new housing units were finsihed in 2022. This ranks #",
                                 region.housing.delta.2022.rank,
                                 " out of ",
                                 housing.since.1992,
                                 " in total annual production since 1992.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Housing construction is robust",
                                          p.caption = housing.growth.caption,
                                          p.chart = housing.change.chart)

# Housing Compared to VISION 2050 -----------------------------------------
observed.housing <- ofm.housing %>% 
  filter(Jurisdiction=="Region" & Variable=="Total Housing Units") %>% 
  filter(Year >=2010) %>%
  select(Year, Variable, Estimate) %>% 
  mutate(Variable="Observed")

observed.forecast <- observed.housing %>% 
  filter(Year>=2010 & Year<2018) %>%
  mutate(Variable="Forecast")

future.housing <- as_tibble(fread('X:/DSA/shiny-uploads/data/regional-housing.csv')) %>% 
  filter(Year>=2018) %>%
  select(Year, Forecast) %>%
  rename(Estimate=Forecast) %>%
  mutate(Variable="Forecast")

vision.housing <- bind_rows(list(observed.housing, observed.forecast, future.housing)) %>%
  mutate(Year=as.character(Year))

housing.vision.2022 <- vision.housing %>% filter(Year=="2022" & Variable=="Forecast") %>% select(Estimate) %>% pull()
housing.observed.2022 <- vision.housing %>% filter(Year=="2022" & Variable=="Observed") %>% select(Estimate) %>% pull()
housing.observed.2022.delta <- housing.observed.2022 - housing.vision.2022
housing.observed.2022.delta.share <- 1 - (housing.observed.2022.delta / housing.vision.2022)

housing.growth.vision.chart <- create.line.chart(t=vision.housing %>% filter(Year<=2025), w.x='Year', w.y='Estimate', w.g='Variable', est.type="number", 
                                                 w.title="Annual Housing Unit Forecast: 2010 to 2025",
                                                 x.type= "Continuous",
                                                 w.lwidth = 3,
                                                 w.breaks = c("2010","2015","2020","2025"),
                                                 w.color = "DkPrLtPr")+
  labs(caption = paste0("Source: Office of Financial Management & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24)) +
  scale_y_continuous(labels = scales::label_comma(), limits=c(1000000,2000000))

housing.vision.caption <- paste0("Between 2018 and 2022, housing growth is in-line with VISION 2050. In VISION 2050 forecasts, total housing units in 2022 were forecasted to be ", 
                                 prettyNum(round(housing.vision.2022,-3), big.mark = ","),
                                 ". The observed housing units in 2022 were  ",
                                 prettyNum(round(housing.observed.2022,-3), big.mark = ","), 
                                 ", a difference of ",
                                 prettyNum(round(housing.observed.2022.delta,-3), big.mark = ","),
                                 " housing units.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Housing production is close to VISION 2050 forecast",
                                          p.caption = housing.vision.caption,
                                          p.chart = housing.growth.vision.chart)

# Housing Growth by Housing Type ------------------------------------------------------------
sf <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Single-Family") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

mf <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Multi-Family") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

mh <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Mobile Home") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

tbl <- bind_rows(list(sf,mf))

mf.housing.delta <- tbl %>% filter(Variable=="Multi-Family") %>% select(Delta) %>% pull() %>% sum()
sf.housing.delta <- tbl %>% filter(Variable=="Single-Family") %>% select(Delta) %>% pull() %>% sum()
mf.housing.share <- mf.housing.delta / (mf.housing.delta+sf.housing.delta)

housing.type.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                                                 w.title="Annual Housing Unit Change by Type: 2013 to 2022",
                                                 w.interactive="no",
                                                 est.type = "number",
                                                 w.pos="stack",
                                                 w.color = "OrPr") +
  labs(caption = paste0("Source: Office of Financial Management")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

housing.type.caption <- paste0("Approximately ", 
                               prettyNum(round(mf.housing.delta,-2), big.mark = ","),
                               " new multi-family housing units were constructed since 2013, ",
                               prettyNum(round(mf.housing.share*100,0), big.mark = ","),
                               "% of the total housing production over the past 10 years.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Most new housing is multi-family",
                                          p.caption = housing.type.caption,
                                          p.chart = housing.type.change.chart)

# Housing Growth by Regional Geography ------------------------------------
tbl <- ofm.housing %>% 
  filter(Filter%in%c(2,4)) %>%
  filter(!(str_detect(Jurisdiction, "Region"))) %>%
  filter(Variable=="Total Housing Units") %>%
  drop_na() %>%
  select(-Filter,-Jurisdiction,-Variable) %>%
  group_by(Year,regional_geography) %>%
  summarize_all(sum) %>%
  as_tibble()

cities <- tbl %>%
  filter(regional_geography=="Cities & Towns") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

core <- tbl %>%
  filter(regional_geography=="Core Cities") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

metro <- tbl %>%
  filter(regional_geography=="Metropolitan Cities") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

hct <- tbl %>%
  filter(regional_geography=="High Capacity Transit Community") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

unincorporated <- tbl %>%
  filter(regional_geography=="Unincorporated") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  mutate(Year = as.character(Year)) %>%
  rename(`Regional Geography`=regional_geography)

ofm.housing.rgeo <- bind_rows(list(metro,core,hct,cities,unincorporated))
rm(tbl,metro,core,hct,cities,unincorporated)

total.housing.growth.2022 <- ofm.housing.rgeo %>% filter(Year=="2022") %>% select(Delta) %>% pull() %>% sum()
metro.housing.growth.2022 <- ofm.housing.rgeo %>% filter(Year=="2022" & `Regional Geography`=="Metropolitan Cities") %>% select(Delta) %>% pull()
core.housing.growth.2022 <- ofm.housing.rgeo %>% filter(Year=="2022" & `Regional Geography`=="Core Cities") %>% select(Delta) %>% pull()
hct.housing.growth.2022 <- ofm.housing.rgeo %>% filter(Year=="2022" & `Regional Geography`=="High Capacity Transit Community") %>% select(Delta) %>% pull()

metro.housing.growth.share.2022 <- metro.housing.growth.2022 / total.housing.growth.2022
top3.housing.growth.share.2022 <- (metro.housing.growth.2022+core.housing.growth.2022+hct.housing.growth.2022) / total.housing.growth.2022

seattle.housing <- ofm.housing %>% filter(Jurisdiction=="Seattle" & Variable=="Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) 

seattle.housing.growth.2022 <- seattle.housing %>% filter(Year==2022) %>% select(Delta) %>% pull()
seattle.housing.growth.share.2022 <- seattle.housing.growth.2022 / total.housing.growth.2022

metro.housing.growth.ten <- ofm.housing.rgeo %>% filter(Year>="2013" & `Regional Geography`=="Metropolitan Cities") %>% select(Delta) %>% sum()
all.housing.growth.ten <- ofm.housing.rgeo %>% filter(Year>="2013") %>% select(Delta) %>% sum()
metro.housing.growth.share.ten <- metro.housing.growth.ten/all.housing.growth.ten

cities.housing <- ofm.housing %>% 
  filter(Filter==4 & Variable=="Total Housing Units") %>%
  filter(Year==2013 | Year==2022) %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year==2022) %>%
  arrange(desc(Delta)) %>%
  mutate(Rank = order(Delta, decreasing = TRUE))

top.five.cities.housing <- cities.housing %>% filter(Rank<=5) %>% select(Jurisdiction) %>% pull()
top.five.cities.housing.growth <- cities.housing %>% filter(Rank<=5) %>% select(Delta) %>% pull() %>% sum()

# Growth in 2022
tbl <- ofm.housing.rgeo %>% filter(Year=="2022") %>% mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT"))
housing.rgeo.chart.22 <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", 
                                              w.title = "Housing Unit Growth by Regional Geography: 2022", 
                                              est.type = "number") +
  labs(caption = paste0("Source: Office of Financial Management")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

housing.by.rgeo.caption <- paste0("Between 2021 and 2022, ",
                                  prettyNum(round(metro.housing.growth.share.2022*100,0), big.mark = ","),
                                  "% of the total housing growth in the region was in Metropolitan Cities.")

housing.by.rgeo.bullets <- paste0("The City of Seattle added ", 
                                  prettyNum(round(seattle.housing.growth.2022,-2), big.mark = ","),
                                  " housing units in 2022, about ",
                                  prettyNum(round(seattle.housing.growth.share.2022*100,0), big.mark = ","),
                                  "% of the total regional housing unit growth last year.\n",
                                  "Approximately ", 
                                  prettyNum(round(metro.housing.growth.ten,-2), big.mark = ","),
                                  " housing units were added to Metropolitan Cities in the last ten years, ",
                                  prettyNum(round(metro.housing.growth.share.ten*100,0), big.mark = ","),
                                  "% of the total regional housing unit growth.\n",
                                  "The five cities that added the most housing in the past ten years were: ", 
                                  paste(top.five.cities.housing,collapse = ", "),
                                  ". These five citites added more than ",
                                  prettyNum(round(top.five.cities.housing.growth,-2), big.mark = ","),
                                  " housing units in the last ten years.")

psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="Big Cities are adding a large share of the housing", 
                                         p.caption=housing.by.rgeo.caption, 
                                         p.bullet=housing.by.rgeo.bullets, 
                                         p.chart=housing.rgeo.chart.22)

# Housing Near High Capacity Transit --------------------------------------
housing.near.hct <- ofm.block.data %>%
  select(HCT_Buffer, estimate_year, housing_units) %>%
  group_by(HCT_Buffer, estimate_year) %>%
  summarise(Housing = sum(housing_units)) %>%
  as_tibble() %>%
  pivot_wider(id_cols=estimate_year, names_from=HCT_Buffer, values_from=Housing) %>%
  rename(Year=estimate_year, NonHCT=No, HCT=Yes) %>%
  mutate(Total=NonHCT+HCT) %>%
  mutate(HCT_Share_Total = HCT/Total) %>%
  mutate(Delta_Total = round(Total - lag(Total),-2)) %>%
  mutate(Delta_HCT = round(HCT - lag(HCT),-2)) %>%
  mutate(HCT_Share_Growth = Delta_HCT/Delta_Total) %>%
  filter(Year>=2013) %>%
  mutate(variable="Housing Unit Growth") %>%
  mutate(Year = as.character(Year))

hct.hu.growth.since.2018 <- housing.near.hct %>% filter(Year >= "2018") %>% select(Delta_HCT) %>% pull() %>% sum()
tot.hu.growth.since.2018 <- housing.near.hct %>% filter(Year >= "2018") %>% select(Delta_Total) %>% pull() %>% sum()
hct.hu.growth.share.since.2018 <- hct.hu.growth.since.2018/tot.hu.growth.since.2018

housing.growth.near.hct.chart <- create.line.chart(t=housing.near.hct, w.x="Year", w.y="HCT_Share_Growth", w.g="variable",
                                                   w.title = "Housing Growth near Current and Future High Capacity Transit: 2013 to 2020", 
                                                   x.type= "Continuous",
                                                   est.type = "percent",
                                                   w.lwidth = 3,
                                                   w.breaks = c("2013","2014","2015","2016","2017","2018","2019","2020"),
                                                   w.color = "PrOr") +
  labs(caption = paste0("Source: Office of Financial Management Small Area Estimates Program")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  scale_y_continuous(labels = scales::label_percent(), limits=c(0,1))

housing.growth.near.hct.caption <- paste0("Between 2018 and 2020, approximately ", 
                                          prettyNum(round(hct.hu.growth.share.since.2018*100,0), big.mark = ","),
                                          "% of housing unit growth has occurred within a half mile of current or planned ",
                                          "high capacity transit.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Over 60% of housing unit growth is near High Capacity Transit",
                                          p.caption = housing.growth.near.hct.caption,
                                          p.chart = housing.growth.near.hct.chart)

# Job Transition --------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Employment Trends",
                                  p.img=('X:/DSA/shiny-uploads/images/rentonboeing3.jpg'))

# Annual Job Changes ------------------------------------------------------
qcew_jobs <- process_qcew_monthly_msa(c.yr=2022, c.mo=7)

tbl <- qcew_jobs %>%
  filter(geography=="Region" & month=="07" & variable=="Total Nonfarm") %>%
  mutate(Delta = round(estimate - lag(estimate),-2)) %>%
  filter(year >=2013) %>%
  mutate(year = as.character(year))

job.change <- create_column_chart(t=tbl, w.x="year", w.y="Delta", f="variable", 
                                  w.color="DkGnLtGn", w.title="Job Change: July 2013 to July 2022",
                                  est.type = "number") +
  labs(caption = paste0("Source: Washington State Employment Securities Department")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

job.loss.2020 <- tbl %>% filter(year=="2020") %>% select(Delta) %>% pull()
job.chng.2122 <- tbl %>% filter(year%in%c("2021","2022")) %>% select(Delta) %>% pull() %>% sum()

job.caption <- paste0("After losing approximately ", 
                      prettyNum(job.loss.2020, big.mark = ","),
                      " jobs between 2019 and 2020 due to COVID-19, the region has added more than ",
                      prettyNum(job.chng.2122, big.mark = ","), 
                      " jobs in the past two years to finally catch up to 2019.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "The region has finally returned to 2019 job levels",
                                          p.caption = job.caption,
                                          p.chart = job.change)

# Job Growth Compared to Vision 2050 -------------------------------
observed.jobs <- qcew_jobs %>%
  filter(geography=="Region" & month=="07" & variable=="Total Nonfarm") %>%
  filter(year >=2010) %>%
  mutate(year = as.character(year))%>% 
  select(year, estimate, variable) %>%
  mutate(variable="Observed")

observed.forecast.jobs <- observed.jobs %>% 
  filter(year<=2018) %>% 
  mutate(variable="Forecast") %>% 
  mutate(year = as.character(year)) %>%
  select(year, estimate, variable)

forecast.jobs <- get_elmer_table("Macroeconomic.employment_facts") %>% 
  filter(employment_sector_dim_id==18 & data_year >= 2019) %>%
  rename(year=data_year,estimate=jobs) %>%
  mutate(variable="Forecast") %>% 
  mutate(year = as.character(year)) %>%
  select(year, estimate, variable)

job.growth.vision <- bind_rows(list(observed.jobs, observed.forecast.jobs, forecast.jobs)) %>% filter(year<=2025) %>% mutate(year=as.character(year))
job.vision.2022 <- job.growth.vision %>% filter(year=="2022" & variable=="Forecast") %>% select(estimate) %>% pull()
job.observed.2022 <- job.growth.vision %>% filter(year=="2022" & variable=="Observed") %>% select(estimate) %>% pull()
job.observed.2022.delta <- job.observed.2022 - job.vision.2022
job.observed.2022.delta.share <- (job.observed.2022.delta / job.vision.2022)

job.growth.vision.chart <- create.line.chart(t=job.growth.vision, w.x='year', w.y='estimate', w.g='variable', est.type="number", 
                                             w.title="Annual Job Forecast: 2010 to 2025",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025"),
                                             w.color = "DkOrLtOr") +
  labs(caption = paste0("Source: Washington State Employment Securities Department & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

job.vision.caption <- paste0("In VISION 2050 forecasts, total wage & salary jobs in 2022 were forecasted to be ", 
                             prettyNum(round(job.vision.2022,-3), big.mark = ","),
                             ". The observed jobs in 2022 were  ",
                             prettyNum(round(job.observed.2022,-3), big.mark = ","), 
                             ", a difference of ",
                             prettyNum(round(job.observed.2022.delta,-3), big.mark = ","),
                             " jobs, a ",
                             prettyNum(round(job.observed.2022.delta.share*100,0), big.mark = ","),
                             "% difference.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Job Growth has rebounded",
                                          p.caption = job.vision.caption,
                                          p.chart = job.growth.vision.chart)

# Job Growth by Sector ----------------------------------------------------
sectors <- c("Construction",
             "Educational and Health Services",
             "Financial Activities",
             "Government",
             "Information",
             "Leisure and Hospitality",
             "Manufacturing",
             "Other Services",
             "Professional and Business Services",
             "Trade, Transportation, and Utilities")

observed.sector.jobs <- qcew_jobs %>%
  filter(geography=="Region" & month=="07" & variable%in%sectors) %>%
  filter(year >=2018) %>%
  mutate(year = as.character(year)) %>%
  mutate(Delta = round(estimate - lag(estimate),-2)) %>%
  filter(year >=2019) %>%
  mutate(str_wrap(variable, width=16))

tbl <- observed.sector.jobs %>% filter(year %in% c("2019","2022"))

job.sectors.chart <- create_facet_bar_chart(t=tbl, w.x="year", w.y="estimate", f="variable", g="variable",
                                                  est.type = "number", 
                                                  w.title = "Jobs by Sector: July 2019 to July 2022",
                                                  w.color = "psrc_light", w.facet = 4) +
  labs(caption = paste0("Source: Washington State Employment Securities Department")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Not all sectors have recovered",
                                  p.chart = job.sectors.chart)

# Transportation Slide Transition------------------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Transportation Trends",
                                  p.img=('X:/DSA/shiny-uploads/images/burien-transit-center.jpg'))

# Non-SOV Mode Modes ------------------------------------------------------
total.work.trips <- c("B08006_001")
da.work.trips <- c("B08006_003")
cp.work.trips <- c("B08006_004")
trn.work.trips <- c("B08006_008")
bke.work.trips <- c("B08006_014")
wlk.work.trips <- c("B08006_015")
oth.work.trips <- c("B08006_016")
wfh.work.trips <- c("B08006_017")

all.trips <- c(total.work.trips, da.work.trips, cp.work.trips, trn.work.trips, bke.work.trips, wlk.work.trips, oth.work.trips, wfh.work.trips)
non.da.trips <- c(cp.work.trips,trn.work.trips,bke.work.trips,wlk.work.trips,wfh.work.trips)

rtp.nonsov.shr <- 0.49
fed.perf.target.nonsov.shr <- 0.333

work.mode <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=c(2010,2015,2020), acs.type = 'acs5') %>% filter(variable %in% all.trips)
mode.totals <- work.mode %>% filter(variable %in% total.work.trips) %>% select(name, year, estimate) %>% rename(total=estimate)
work.mode <- left_join(work.mode, mode.totals, by=c("name","year")) %>% mutate(share=estimate/total)

tbl <- work.mode %>%
  filter(variable %in% non.da.trips) %>%
  filter(name=="Region") %>%
  select(name, year, share) %>%
  group_by(name, year) %>%
  summarise(share=sum(share)) %>%
  as_tibble() %>%
  mutate(year=as.character(year))

non.sov.ms.chart <- create_column_chart(t=tbl, w.x="year", w.y="share", f="name", 
                                        w.color = "psrc_light",
                                        w.title="Non-SOV Mode to Work: 2010, 2015 and 2020",
                                        h.ref = c(fed.perf.target.nonsov.shr, rtp.nonsov.shr),
                                        h.ref.nm = c("", ""),
                                        h.ref.cl = c(psrc_colors$psrc_oranges[3], psrc_colors$psrc_purples[3])) +
  labs(caption = paste0("Source: American Community Survey 5yr Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.position = "none") +
  annotate("text", x = 1, y = fed.perf.target.nonsov.shr+.01, label = "Federal Performance Target", size = 10, family="Poppins") +
  annotate("text", x = 1, y = rtp.nonsov.shr+.01, label = "2050 Forecast", size = 10, family="Poppins")

non.sov.ms.2010 <- tbl %>% filter(year=="2010") %>% select(share) %>% pull()
non.sov.ms.2020 <- tbl %>% filter(year=="2020") %>% select(share) %>% pull()
transit.ms.2020 <- work.mode %>% filter(name=="Region" & year=="2020" & variable %in% trn.work.trips) %>% select(share) %>% pull()
wfh.ms.2010 <- work.mode %>% filter(name=="Region" & year=="2010" & variable %in% wfh.work.trips) %>% select(share) %>% pull()
wfh.ms.2020 <- work.mode %>% filter(name=="Region" & year=="2020" & variable %in% wfh.work.trips) %>% select(share) %>% pull()

non.sov.ms.caption <- paste0("In 2020, ",
                             prettyNum(round(non.sov.ms.2020*100,0), big.mark = ","),
                             "% of all work trips were in a Non-SOV mode, up from ",
                             prettyNum(round(non.sov.ms.2010*100,0), big.mark = ","),
                             "% in 2010.")

non.sov.ms.bullets <- paste0("In 2020, the region met the Federal Performance Target for Non-SOV mode share of at least ", 
                             prettyNum(round(fed.perf.target.nonsov.shr*100,0), big.mark = ","),
                             "%\n",
                             "Approximately ", 
                             prettyNum(round(transit.ms.2020*100,0), big.mark = ","),
                             "% of work trips used transit in 2020\n",
                             "Approximately ", 
                             prettyNum(round(wfh.ms.2020*100,0), big.mark = ","),
                             "% of all workers worked from Home in 2020, up from ",
                             prettyNum(round(wfh.ms.2010*100,0), big.mark = ","),
                             "% in 2010.")

psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="Fewer people are driving alone to work", 
                                         p.caption=non.sov.ms.caption, 
                                         p.bullet=non.sov.ms.bullets, 
                                         p.chart=non.sov.ms.chart)

# Non-SOV Share by Metro -----------------------------------------------------
metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(NonSOV_Share)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

nonsov.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="NonSOV_Share", 
                                          f="plot_id", w.color = "psrc_light",
                                          w.title="Non-SOV Share to Work by MPO: 2020") +
  labs(caption = paste0("Source: 2016-2020 American Community Survey 5yr Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "The region has one of the lowest SOV Commute shares",
                                  p.chart = nonsov.mpo.chart)

# Transit Boardings compared to RTP-------------------------------------------------------
transit.annual <- process_ntd_year_to_date_data(c.yr="2021")

observed.boardings <- transit.annual %>% 
  filter(variable == "Total Transit" & geography=="Region" & concept=="Transit Boardings") %>% 
  mutate(variable = "Observed") %>% 
  filter(year>="2010") %>%
  select(year, estimate, variable)

observed.forecast.boardings <- observed.boardings %>% 
  filter(year>=2010 & year<=2018) %>% 
  mutate(variable="Forecast") %>%
  select(year, estimate, variable)

forecast.boardings <- fread('X:/DSA/shiny-uploads/data/rtp-transit-boardings.csv') %>%
  mutate(year=as.character(year)) %>% 
  select(year, estimate, variable)

boardings.growth.rtp <- bind_rows(list(observed.boardings, observed.forecast.boardings, forecast.boardings)) %>% filter(year<=2025) %>% mutate(year=as.character(year))

observed.boardings.2021 <- boardings.growth.rtp %>% filter(variable=="Observed" & year=="2021") %>% select(estimate) %>% pull()
forecast.boardings.2021 <- boardings.growth.rtp %>% filter(variable=="Forecast" & year=="2021") %>% select(estimate) %>% pull()
observed.boardings.2021.ratio <- observed.boardings.2021/forecast.boardings.2021

boardings.growth.rtp.chart <- create.line.chart(t=boardings.growth.rtp, w.x='year', w.y='estimate', w.g='variable', est.type="number", 
                                                w.title="Annual Transit Boardings: 2010 to 2025",
                                                x.type= "Continuous",
                                                w.lwidth = 3,
                                                w.breaks = c("2010","2015","2020","2025"),
                                                w.color = "DkPrLtPr")+
  labs(caption = paste0("Source: National Transit Database & PSRC 2022-2050 RTP")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24)) +
  scale_y_continuous(labels = scales::label_comma(), limits=c(0,350000000))

transit.rtp.caption <- paste0("Between 2018 and 2022, transit boardings were reduced significantly as a result of the global pandemic. In 2021, ", 
                              prettyNum(round(observed.boardings.2021,-3), big.mark = ","),
                              " boardings happened on the regional transit system, approximately ",
                              prettyNum(round(observed.boardings.2021.ratio*100,0), big.mark = ","), 
                              "% of the forecasted boardings from the RTP in the same year.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "The pandemic has had a major impact on transit usage",
                                          p.caption = transit.rtp.caption,
                                          p.chart = boardings.growth.rtp.chart)

# Monthly Transit Boardings ------------------------------------------
transit.monthly <- process_ntd_monthly_data()

tbl <- transit.monthly %>%
  filter(geography=="Region" & variable=="Total Transit" & concept=="Transit Boardings" & year >="2019")

ytd.boardings.2019 <- tbl %>% filter(month<="06" & year=="2019") %>% select(estimate) %>% pull() %>% sum()
ytd.boardings.2021 <- tbl %>% filter(month<="06" & year=="2021") %>% select(estimate) %>% pull() %>% sum()
ytd.boardings.2022 <- tbl %>% filter(month<="06" & year=="2022") %>% select(estimate) %>% pull() %>% sum()

ytd.boardings.2022.to.2021 <- ytd.boardings.2022/ytd.boardings.2021
ytd.boardings.2022.to.2019 <- ytd.boardings.2022/ytd.boardings.2019

monthly.transit.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                           w.title="Monthly Transit Boardings: 2019 to 2022", 
                                           d.form = "%B", w.lwidth=3,
                                           w.color="psrc_light") +
  labs(caption = paste0("Source: National Transit Database")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

transit.ytd.caption <- paste0("Transit boardings in the first half of 2022 were up ", 
                              prettyNum(round((ytd.boardings.2022.to.2021-1)*100,0), big.mark = ","),
                              "% compared to the first half of 2021. Despite the strong ridership growth, overall ridership is still down ",
                              prettyNum(round((ytd.boardings.2022.to.2019-1)*100,0), big.mark = ","), 
                              "% compared to the first six months of 2019.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Transit usage continues to grow",
                                          p.caption = transit.ytd.caption,
                                          p.chart = monthly.transit.chart)

# Fatal Collisions --------------------------------------------------------
fars_data <- NULL
for (y in seq(2000,census.yr,by=1)) {
  
  # Open Current Years FARS Accident Data
  all_files <- as.character(unzip(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), list = TRUE)$Name)
  
  c <- read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1])) %>%
    mutate(COUNTY_FIPS=str_pad(COUNTY, width=3, side=c("left"), pad="0")) %>%
    mutate(STATE_FIPS=str_pad(STATE, width=2, side=c("left"), pad="0")) %>%
    mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS)) %>%
    filter(GEOID %in% c("53033","53035","53053","53061")) %>%
    select(GEOID, FATALS) %>%
    mutate(FATAL_COLLISIONS = 1) %>%
    group_by(GEOID) %>%
    summarise(Fatalities=sum(FATALS), Fatal_Collisions =sum(FATAL_COLLISIONS)) %>%
    mutate(year=y)
    as_tibble
  
  ifelse(is.null(fars_data), fars_data <- c, fars_data <- bind_rows(fars_data,c))
  
  rm(c)
  
}

region_fars_data <- fars_data %>%
  select(-GEOID) %>% 
  group_by(year) %>%
  summarise(Fatalities=sum(Fatalities), Fatal_Collisions =sum(Fatal_Collisions)) %>%
  as_tibble %>%
  mutate(variable="Region")

fatal.collisions.chart <- create_column_chart(t=region_fars_data, w.x="year", w.y="Fatal_Collisions", f="variable", 
                                  w.color="psrc_dark", w.title="Annual Fatal Collisons: 2000 to 2020",
                                  est.type = "number") +
  labs(caption = paste0("Source: Fatality Analysis Reporting System (FARS) 2000 to 2020")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")


psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Over 200 fatal collision happen every year",
                                  p.chart = fatal.collisions.chart)

# Fatality Rates by Metro -----------------------------------------------------
metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(Fatal_Collision_Rate)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

fatalities.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="Fatal_Collision_Rate", 
                                     f="plot_id", w.color = "psrc_light",
                                     est.type = "number",
                                     w.title="Fatal Collisions per 100,000 people by MPO: 2020") +
  labs(caption = paste0("Source:2016-2020 American Community Survey 5yr Data & FARS National Data 2020")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "The region has one of the lowest Fatal Accident Rates",
                                  p.chart = fatalities.mpo.chart)


# Question Slide ------------------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Questions: chelmann@psrc.org",
                                  p.img=('X:/DSA/shiny-uploads/images/rhs-slide-2400x800.jpg'))

# Write Out Slides to Disc ------------------------------------------------
print(psrc_pres, target = "puget-sound-trends-gmpb-sept-2022.pptx") 



