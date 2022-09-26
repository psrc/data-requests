library(psrctrends)
library(psrcplot)
library(psrcslides)
library(psrccensus)

library(tidyverse)
library(lubridate)
library(sf)
library(officer)
library(data.table)
library(openxlsx)

install_psrc_fonts()
psrc_pres = read_pptx(system.file('extdata', 'psrc-trends-template.pptx', package='psrcslides'))

census_yrs <- c(seq(2010,2019,by=1),2021)
mpo_results = NULL

# Data Used for Slides ----------------------------------------------------
ofm.pop <- get_ofm_intercensal_population()
ofm.pop.chg <- get_ofm_postcensal_pop_change()

# People of Color by Metropolitan Region
calc_by_acs <- partial(calculate_mpo_people_of_color, acs.type="acs1")
mpo_results[["People of Color"]][["acs1"]] <- map(census_yrs, calc_by_acs)
mpo_results[["People of Color"]][["acs1"]] <- set_names(mpo_results[["People of Color"]][["acs1"]], census_yrs)
mpo_results[["People of Color"]][["acs5"]] <- calculate_mpo_people_of_color(census.yr = 2020, acs.type = "acs5")

ofm.housing <- get_ofm_postcensal_housing()

# Home Ownership by Metropolitan Region
calc_by_acs <- partial(calculate_mpo_ownership_share, acs.type="acs1")
mpo_results[["Home Ownership"]][["acs1"]] <- map(census_yrs, calc_by_acs)
mpo_results[["Home Ownership"]][["acs1"]] <- set_names(mpo_results[["Home Ownership"]][["acs1"]], census_yrs)
mpo_results[["Home Ownership"]][["acs5"]] <- calculate_mpo_ownership_share(census.yr = 2020, acs.type = "acs5")

qcew_jobs <- process_qcew_monthly_msa(c.yr=2022, c.mo=7)

airport.operations <- process_sea_operations_data(c.yr=2022, c.mo=7)

transit.annual <- process_ntd_year_to_date_data(c.yr="2021")

transit.ytd <- process_ntd_year_to_date_data(c.yr = '2022')

# Cover Slide -------------------------------------------------------------
psrc_pres <- add_cover_slide(p = psrc_pres,
                             p.title = "PSRC Trends",
                             p.mtg = "Executive Board",
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
tbl <- ofm.pop %>% 
  filter(Jurisdiction=="Region") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

pop.growth <- tbl %>% filter(Year=="2022") %>% select(Delta) %>% pull()
pop.ovr.50k <- tbl %>% filter(Delta>=50000) %>% select(Delta) %>% pull() %>% length()

pop.change <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
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
observed.pop <- ofm.pop %>% 
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
poc <- map(mpo_results[["People of Color"]][["acs1"]], ~filter(.x, MPO_AREA=="Seattle" & variable=="People of Color Share" )) %>% bind_rows()

poc <- poc %>%
  mutate(equiv_day = lubridate::ymd(paste0(year,"-01-01")))

pop.by.race.chart <- create.line.chart(t=poc, 
                                       w.x='equiv_day', w.y='estimate', w.g='variable', est.type="percent", 
                                       w.title="People of Color: 2010 to 2021",
                                       x.type= "Date",
                                       d.form="%Y",
                                       w.lwidth = 3,
                                       w.color = "LtPrDkPr") +
  labs(caption = paste0("Source: American Community Survey Data Table B03002")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0.2,0.5))

white.alone <- c("B03002_003")
people.of.color <- c("B03002_004","B03002_005","B03002_006","B03002_007","B03002_008","B03002_009","B03002_012")

pop.by.race <- get_acs_recs(geography = 'county',table.names = c('B03002'), years=c(2021), acs.type = 'acs1')

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

poc.2021 <- tbl %>% filter(year=="2021" & POC=="Person of Color" & name=="Region") %>% select(share) %>% pull()
poc.king.2021 <- tbl %>% filter(year=="2021" & POC=="Person of Color" & name=="King County") %>% select(share) %>% pull()

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

indigenous.tot <- tbl %>% filter(year=="2021" & name=="Region" & variable=="B03002_005") %>% select(estimate) %>% pull()
indigenous.shr <- tbl %>% filter(year=="2021" & name=="Region" & variable=="B03002_005") %>% select(share) %>% pull()

asian.tot <- tbl %>% filter(year=="2021" & name=="Region" & variable=="B03002_006") %>% select(estimate) %>% pull()
asian.shr <- tbl %>% filter(year=="2021" & name=="Region" & variable=="B03002_006") %>% select(share) %>% pull()


pop.by.race.caption <- paste0("In 2021, People of Color accounted for approximately ",
                              prettyNum(round(poc.2021*100,0), big.mark = ","),
                              "% of the total population in the region")

pop.by.race.bullets <- paste0("In 2021 there were approximately ", 
                              prettyNum(round(indigenous.tot,-2), big.mark = ","),
                              " Indigenous people living in the Puget Sound region.\n",
                              "Approximately ", 
                              prettyNum(round(asian.shr*100,1), big.mark = ","),
                              "% of the total population in the region were of Asian descent.\n",
                              "People of Color accounted for approximately ", 
                              prettyNum(round(poc.king.2021*100,0), big.mark = ","),
                              "% of the total population in King County, the largest share of any county.")
  
psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="The region continues to diversify", 
                                         p.caption=pop.by.race.caption, 
                                         p.bullet=pop.by.race.bullets, 
                                         p.chart=pop.by.race.chart)

# MPO Comparison Data ----------------------------------------------------------------
metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

t1 <- mpo_results[["People of Color"]][["acs5"]] %>% filter(MPO_AREA!="Seattle" & variable=="People of Color Share")
t2 <- mpo_results[["People of Color"]][["acs1"]][["2021"]] %>% filter(MPO_AREA=="Seattle" & variable=="People of Color Share")

tbl <- bind_rows(t1,t2) %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(estimate)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

pop.by.race.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="estimate", 
                                          f="plot_id", w.color = "psrc_light",
                                          w.title="People of Color by MPO") +
  labs(caption = paste0("Source: American Community Survey Data Table B03002")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "The region is less diverse than many Metro Areas",
                                  p.chart = pop.by.race.mpo.chart)

rm(t1,t2,tbl)

# Regional Geography Population Growth ------------------------------------
tbl <- ofm.pop %>% 
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

seattle <- ofm.pop %>% filter(Jurisdiction=="Seattle") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) 

seattle.pop.growth.2022 <- seattle %>% filter(Year==2022) %>% select(Delta) %>% pull()
seattle.pop.growth.share.2022 <- seattle.pop.growth.2022 / total.pop.growth.2022

metro.pop.growth.ten <- ofm.pop.rgeo %>% filter(Year>="2013" & `Regional Geography`=="Metropolitan Cities") %>% select(Delta) %>% sum()
all.pop.growth.ten <- ofm.pop.rgeo %>% filter(Year>="2013") %>% select(Delta) %>% sum()

metro.pop.growth.share.ten <- metro.pop.growth.ten/all.pop.growth.ten

cities <- ofm.pop %>% 
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
                                 " new housing units were finsihed in 2022, the second most since 1992.")

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

housing.vision.caption <- paste0("Between 2018 and 2022, housing growth is a bit lower than VISION 2050 forecasts. In VISION 2050 forecasts, total housing units in 2022 were forecasted to be ", 
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

# Home Ownership by Region ------------------------------------------------------
own <- map(mpo_results[["Home Ownership"]][["acs1"]], ~filter(.x, MPO_AREA=="Seattle" & variable=="Owner Occupied Housing Units Share")) %>% bind_rows()
own.2021 <- own %>% filter(year==2021) %>% select(estimate) %>% pull()

own <- own %>%
  mutate(equiv_day = lubridate::ymd(paste0(year,"-01-01")))

ownership.chart <- create.line.chart(t=own, 
                                     w.x='equiv_day', w.y='estimate', w.g='variable', est.type="percent", 
                                     w.title="Home Ownership: 2010 to 2021",
                                     x.type= "Date",
                                     d.form="%Y",
                                     w.lwidth = 3,
                                     w.color = "LtPrDkPr") +
  labs(caption = paste0("Source: American Community Survey Data Table B25003")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0.25,0.75))

# Home Ownership By African American Householders
own.black <- get_acs_recs(geography = 'county',table.names = c('B25003B'), years=c(2021), acs.type = 'acs1')
tot <- own.black %>% filter(name=="Region" & variable=="B25003B_001") %>% select(estimate) %>% pull()
owners <- own.black %>% filter(name=="Region" & variable=="B25003B_002") %>% select(estimate) %>% pull()
own.black.2021<- owners/tot

# Home Ownership By Hispanic Householders
own.hispanic <- get_acs_recs(geography = 'county',table.names = c('B25003I'), years=c(2021), acs.type = 'acs1')
tot <- own.hispanic %>% filter(name=="Region" & variable=="B25003I_001") %>% select(estimate) %>% pull()
owners <- own.hispanic %>% filter(name=="Region" & variable=="B25003I_002") %>% select(estimate) %>% pull()
own.hispanic.2021<- owners/tot

# Home Ownership By Indigenous Householders
own.indigenous <- get_acs_recs(geography = 'county',table.names = c('B25003C'), years=c(2021), acs.type = 'acs1')
tot <- own.indigenous %>% filter(name=="Region" & variable=="B25003C_001") %>% select(estimate) %>% pull()
owners <- own.indigenous %>% filter(name=="Region" & variable=="B25003C_002") %>% select(estimate) %>% pull()
own.indigenous.2021<- owners/tot

# Home Ownership By White Householders
own.white <- get_acs_recs(geography = 'county',table.names = c('B25003H'), years=c(2021), acs.type = 'acs1')
tot <- own.white %>% filter(name=="Region" & variable=="B25003H_001") %>% select(estimate) %>% pull()
owners <- own.white %>% filter(name=="Region" & variable=="B25003H_002") %>% select(estimate) %>% pull()
own.white.2021<- owners/tot

ownership.caption <- paste0("In 2021, ",
                              prettyNum(round(own.2021*100,0), big.mark = ","),
                              "% of households owned their own home in the region but there are noticable disparities by race.")

ownership.bullets <- paste0("Approximately ", 
                            prettyNum(round(own.black.2021*100,0), big.mark = ","),
                            "% of African American households owned their own home.\n",
                            "Approximately ", 
                            prettyNum(round(own.hispanic.2021*100,0), big.mark = ","),
                            "% of Hispanic or Latinx households owned their own home.\n",
                            "Approximately ", 
                            prettyNum(round(own.indigenous.2021*100,0), big.mark = ","),
                            "% of Indigenous households owned their own home. \n",
                            "Approximately ", 
                            prettyNum(round(own.white.2021*100,0), big.mark = ","),
                            "% of Non-Hispanic White households owned their own home.")

psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="Over 60% of people own their own home in the region", 
                                         p.caption=ownership.caption, 
                                         p.bullet=ownership.bullets, 
                                         p.chart=ownership.chart)

# Ownership MPO Comparison Data ----------------------------------------------------------------
t1 <- mpo_results[["Home Ownership"]][["acs5"]] %>% filter(MPO_AREA!="Seattle" & variable=="Owner Occupied Housing Units Share")
t2 <- mpo_results[["Home Ownership"]][["acs1"]][["2021"]] %>% filter(MPO_AREA=="Seattle" & variable=="Owner Occupied Housing Units Share")

tbl <- bind_rows(t1,t2) %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(estimate)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

ownership.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="estimate", 
                                          f="plot_id", w.color = "psrc_light",
                                          w.title="Home Ownership by MPO") +
  labs(caption = paste0("Source: American Community Survey Data Table B25003")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Home Ownership in the region is in the middle of the pack",
                                  p.chart = ownership.mpo.chart)

rm(t1,t2,tbl)

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

job.growth.vision <- bind_rows(list(observed.jobs, observed.forecast.jobs, forecast.jobs)) %>% mutate(year=as.character(year))

job.growth.vision.chart <- create.line.chart(t=job.growth.vision, w.x='year', w.y='estimate', w.g='variable', est.type="number", 
                                             w.title="Annual Job Forecast: 2010 to 2025",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025","2030", "2035", "2040", "2045", "2050"),
                                             w.color = "DkOrLtOr") +
  labs(caption = paste0("Source: Washington State Employment Securities Department & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))+
  scale_y_continuous(labels = scales::label_comma(), limits=c(1500000,3500000))

psrc_pres <- add_full_chart_slide(p = psrc_pres,
                                  p.title = "The region is about 1 year behind job forecasts for 2022",
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
        axis.text.x = element_text(size=24), 
        axis.text.y = element_text(size=24),
        legend.position = "none",
        strip.text = element_text(size = 28))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Not all job sectors have fully recovered",
                                  p.chart = job.sectors.chart)

# Unemployment Rate -------------------------------------------------------
unemployment <- NULL
geos <- c("Seattle MD (King - Snoh)", "Tacoma-Lakewood (Pierce)", "Bremerton-Silverdale (Kitsap) ")

for (areas in geos) {
  
  t <- as_tibble(read.xlsx('X:/DSA/shiny-uploads/data/LAUS-historical-SA5-22.xlsx', sheet = areas, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>%
    select(-AVERAGE) %>%
    rename(variable=X1) %>%
    mutate(year = variable) %>%
    mutate(year = gsub("Civilian Labor Force","",year)) %>%
    mutate(year = gsub("  Total Employment","",year)) %>%
    mutate(year = gsub("  Total Unemployment","",year)) %>%
    mutate(year = gsub("  Unemployment Rate","",year)) %>%
    mutate(year = as.numeric(year)) %>%
    fill(year) %>%
    drop_na() %>%
    mutate(variable = trimws(variable,which=c("both"))) %>%
    mutate(JAN = as.numeric(JAN)) %>%
    mutate(FEB = as.numeric(FEB)) %>%
    mutate(MAR = as.numeric(MAR)) %>%
    mutate(APR = as.numeric(APR)) %>%
    mutate(MAY = as.numeric(MAY)) %>%
    mutate(JUN = as.numeric(JUN)) %>%
    mutate(JUL = as.numeric(JUL)) %>%
    mutate(AUG = as.numeric(AUG)) %>%
    mutate(SEP = as.numeric(SEP)) %>%
    mutate(OCT = as.numeric(OCT)) %>%
    mutate(NOV = as.numeric(NOV)) %>%
    mutate(DEC = as.numeric(DEC)) %>%
    pivot_longer(!c(variable,year)) %>%
    mutate(geography=areas)
  
  ifelse(is.null(unemployment), unemployment <- t, unemployment <- bind_rows(unemployment,t))
  rm(t)
}

region <- unemployment %>%
  select(-geography) %>%
  group_by(variable,year,name) %>%
  summarise(value=sum(value)) %>%
  mutate(geography="PSRC") %>%
  filter(variable != "Unemployment Rate")

lf <- region %>% filter(variable == "Civilian Labor Force") %>% rename(lf=value) %>% select(-variable) 
ue <- region %>% filter(variable == "Total Unemployment") %>% rename(ue=value) %>% select(-variable) 
ur <- left_join(lf, ue, by=c("year","name","geography")) %>% 
  mutate(value=ue/lf) %>%
  as_tibble() %>%
  select(year,name,value,geography) %>%
  mutate(variable="Unemployment Rate")

unemployment <- bind_rows(list(unemployment, region,ur))

tbl <- unemployment %>% 
  filter(variable=="Unemployment Rate" & geography=="PSRC" & year>=2010) %>%
  mutate(equiv_day=mdy(paste0(name,"-01-",year)))

unemployment.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='value', w.g='variable', est.type="percent", 
                                        w.title="Unemployment Rate: 2010 to 2022",
                                        w.lwidth = 3,
                                        d.form="%Y",
                                        w.color = "DkOrLtOr") +
  labs(caption = paste0("Source: Washington State Employment Securities Department")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Unemployment is back to pre-pandemic levels",
                                  p.chart = unemployment.chart)

# Work from Home ------------------------------------------------------
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

work.mode <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=census_yrs, acs.type = 'acs1') %>% filter(variable %in% all.trips)
work.mode.2020 <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=2020, acs.type = 'acs5') %>% filter(variable %in% all.trips)
work.mode <-bind_rows(work.mode,work.mode.2020)
mode.totals <- work.mode %>% filter(variable %in% total.work.trips) %>% select(name, year, estimate) %>% rename(total=estimate)
work.mode <- left_join(work.mode, mode.totals, by=c("name","year")) %>% mutate(share=estimate/total)

tbl <- work.mode %>%
  filter(variable %in% wfh.work.trips) %>%
  filter(name=="Region") %>%
  select(name, year, share) %>%
  mutate(year=as.character(year))

wfh.chart <- create_column_chart(t=tbl, w.x="year", w.y="share", f="name", 
                                 w.color = "psrc_light",
                                 w.title="Work from Home: 2010 to 2021") +
  labs(caption = paste0("Source: ACS Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Working from home peaked in 2021",
                                  p.chart = wfh.chart)

# Work from Home by MPO ------------------------------------------------------
mpo.file <- system.file('extdata', 'regional-councils-counties.csv', package='psrctrends')
wfh.trips <- c("B08006_001","B08006_017")
work.mode <- tidycensus::get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = wfh.trips, year = 2021, survey = 'acs1') %>% mutate(GEOID=as.character(GEOID))

mpo <- readr::read_csv(mpo.file, show_col_types = FALSE) %>% 
  dplyr::mutate(COUNTY_FIPS=stringr::str_pad(.data$COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
  dplyr::mutate(STATE_FIPS=stringr::str_pad(.data$STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
  dplyr::mutate(GEOID = paste0(.data$STATE_FIPS,.data$COUNTY_FIPS)) %>% 
  mutate(MSA_FIPS=as.character(MSA_FIPS))

wfh.mpo <- left_join(work.mode, mpo, by=c("GEOID"="MSA_FIPS")) %>%
  drop_na() %>%
  select(MPO_AREA, MPO_FIPS, variable, estimate) %>%
  group_by(MPO_AREA, MPO_FIPS, variable) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble %>%
  pivot_wider(names_from=c("variable"), values_from = c("estimate")) %>%
  mutate(share=B08006_017/B08006_001)

tbl <- wfh.mpo %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(share)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

wfh.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="share", 
                                        f="plot_id", w.color = "psrc_light",
                                        w.title="Work from Home by MPO") +
  labs(caption = paste0("Source: American Community Survey Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Work from home in the region was one of the highest during the pandemic",
                                  p.chart = wfh.mpo.chart)

# Airport Slide Transition------------------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Airport Trends",
                                  p.img=('X:/DSA/shiny-uploads/images/seatac-airport-2.jpg'))

# SeaTac Airport Passenger Volumes -----------------------------------------------------
tbl <- airport.operations %>% filter(variable == "PASSENGER GRAND TOTAL")
latest.seatac.month <- tbl %>% filter(year=="2022") %>% select(month) %>% mutate(month=as.integer(month)) %>% max()
latest.seatac.passengers <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2022" & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.passengers.2019 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2019"  & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.passengers.2021 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2021"  & month==latest.seatac.month) %>% select(estimate) %>% pull()

latest.seatac.passengers.ratio <- latest.seatac.passengers/seatac.passengers.2019
latest.seatac.passengers.ratio.2021 <- latest.seatac.passengers/seatac.passengers.2021

seatac.passengers.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                             w.title="Monthly Enplanements: 2019 to 2022", 
                                             w.sub.title="Source: Port of Seattle SEA Airport Statistics",
                                             d.form = "%B", w.lwidth=3,
                                             w.color="psrc_light") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

seatac.passenger.caption <- paste0("Passenger volumes were ", 
                                   prettyNum(round((latest.seatac.passengers.ratio)*100,0), big.mark = ","),
                                   "% of pre-pandemic levels in July 2022.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Passenger Volumes are rebounding",
                                          p.caption = seatac.passenger.caption,
                                          p.chart = seatac.passengers.chart)

# SeaTac Airport Operations -----------------------------------------------------
tbl <- airport.operations %>% filter(variable == "OPERATIONS GRAND TOTAL") %>% mutate(estimate=replace(estimate,estimate==0,33000))
latest.seatac.month <- tbl %>% filter(year=="2022") %>% select(month) %>% mutate(month=as.integer(month)) %>% max()
latest.seatac.operations <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2022" & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.operations.2019 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2019"  & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.operations.2021 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2021"  & month==latest.seatac.month) %>% select(estimate) %>% pull()

latest.seatac.operations.ratio <- latest.seatac.operations/seatac.operations.2019
latest.seatac.operations.ratio.2021 <- latest.seatac.operations/seatac.operations.2021

seatac.operations.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                             w.title="Monthly Aircraft Operations: 2019 to 2022", 
                                             w.sub.title="Source: Port of Seattle SEA Airport Statistics",
                                             d.form = "%B", w.lwidth=3,
                                             w.color="psrc_light") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

seatac.operations.caption <- paste0("Aircraft operations were ", 
                                    prettyNum(round((latest.seatac.operations.ratio)*100,0), big.mark = ","),
                                    "% of pre-pandemic levels in July 2022.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Aircraft Operations are rebounding",
                                          p.caption = seatac.operations.caption,
                                          p.chart = seatac.operations.chart)

# SeaTac Airport Freight -----------------------------------------------------
tbl <- airport.operations %>% filter(variable == "CARGO GRAND TOTAL")
latest.seatac.month <- tbl %>% filter(year=="2022") %>% select(month) %>% mutate(month=as.integer(month)) %>% max()
latest.seatac.cargo <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2022" & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.cargo.2019 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2019"  & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.cargo.2021 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2021"  & month==latest.seatac.month) %>% select(estimate) %>% pull()

latest.seatac.cargo.ratio <- latest.seatac.cargo/seatac.cargo.2019
latest.seatac.cargo.ratio.2021 <- latest.seatac.cargo/seatac.cargo.2021

seatac.cargo.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                             w.title="Monthly Air Cargo: 2019 to 2022", 
                                             w.sub.title="Source: Port of Seattle SEA Airport Statistics",
                                             d.form = "%B", w.lwidth=3,
                                             w.color="psrc_light") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))+
  scale_y_continuous(labels = scales::label_comma(), limits=c(20000,50000))

seatac.cargo.caption <- paste0("Air Cargo volumes were ", 
                                    prettyNum(round((latest.seatac.cargo.ratio)*100,0), big.mark = ","),
                                    "% of pre-pandemic levels in July 2022.")

psrc_pres <- add_full_chart_caption_slide(p = psrc_pres, 
                                          p.title = "Air Cargo is down in 2022",
                                          p.caption = seatac.cargo.caption,
                                          p.chart = seatac.cargo.chart)

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

work.mode <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=census_yrs, acs.type = 'acs1') %>% filter(variable %in% all.trips)
work.mode.2020 <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=2020, acs.type = 'acs5') %>% filter(variable %in% all.trips)
work.mode <-bind_rows(work.mode,work.mode.2020)
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
                                        w.color = "psrc_dark",
                                        w.title="Non-SOV Mode to Work: 2010 to 2021",
                                        h.ref = c(fed.perf.target.nonsov.shr, rtp.nonsov.shr),
                                        h.ref.nm = c("", ""),
                                        h.ref.cl = c(psrc_colors$psrc_oranges[3], psrc_colors$psrc_purples[3])) +
  labs(caption = paste0("Source: American Community Survey Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.position = "none") +
  annotate("text", x = 4, y = fed.perf.target.nonsov.shr+.01, label = "Federal Performance Target", size = 10, family="Poppins") +
  annotate("text", x = 4, y = rtp.nonsov.shr+.01, label = "2050 Forecast", size = 10, family="Poppins")

non.sov.ms.2010 <- tbl %>% filter(year=="2010") %>% select(share) %>% pull()
non.sov.ms.2021 <- tbl %>% filter(year=="2021") %>% select(share) %>% pull()
transit.ms.2021 <- work.mode %>% filter(name=="Region" & year=="2021" & variable %in% trn.work.trips) %>% select(share) %>% pull()

sov.ms.2010 <- work.mode %>% filter(name=="Region" & year=="2010" & variable %in% da.work.trips) %>% select(share) %>% pull()
sov.ms.2021 <- work.mode %>% filter(name=="Region" & year=="2021" & variable %in% da.work.trips) %>% select(share) %>% pull()

non.sov.ms.caption <- paste0("In 2021, ",
                             prettyNum(round(non.sov.ms.2021*100,0), big.mark = ","),
                             "% of all work trips were in a Non-SOV mode, up from ",
                             prettyNum(round(non.sov.ms.2010*100,0), big.mark = ","),
                             "% in 2010.")

non.sov.ms.bullets <- paste0("In 2020, the region met the Federal Performance Target for Non-SOV mode share of at least ", 
                             prettyNum(round(fed.perf.target.nonsov.shr*100,0), big.mark = ","),
                             "%\n",
                             "Approximately ", 
                             prettyNum(round(transit.ms.2021*100,0), big.mark = ","),
                             "% of work trips used transit in 2021\n",
                             "Approximately ", 
                             prettyNum(round(sov.ms.2021*100,0), big.mark = ","),
                             "% of all workers drove alone to work in 2021, down from ",
                             prettyNum(round(sov.ms.2010*100,0), big.mark = ","),
                             "% in 2010.")

psrc_pres <- add_bullet_plus_chart_slide(p=psrc_pres, 
                                         p.title="Fewer people are driving alone to work", 
                                         p.caption=non.sov.ms.caption, 
                                         p.bullet=non.sov.ms.bullets, 
                                         p.chart=non.sov.ms.chart)

# Transit Boardings compared to RTP-------------------------------------------------------
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

# Transit MPO Comparison Data ----------------------------------------------------------------
mpo.transit <- calculate_mpo_transit()
mpo.boardings.2019 <- mpo.transit %>% filter(variable=="Total Boardings" & year==2019) %>% select(MPO_AREA,estimate) %>% rename(Boardings_2019=estimate)
mpo.boardings.2022 <- mpo.transit %>% filter(variable=="Total Boardings" & year==2022) %>% select(MPO_AREA,estimate) %>% rename(Boardings_2022=estimate)
mpo.boardings <- left_join(mpo.boardings.2019,mpo.boardings.2022,by=c("MPO_AREA")) %>% mutate(ratio=Boardings_2022/Boardings_2019)

tbl <- mpo.boardings %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(ratio)

mpo_order <- tbl %>% select(MPO_AREA) %>% pull()
tbl <- tbl %>% mutate(MPO_AREA = factor(x=MPO_AREA, levels=mpo_order))

transit.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_AREA", w.y="ratio", 
                                  f="plot_id", w.color = "psrc_light",
                                  w.title="Ratio of 2022 Transit Ridership to Pre-Pandemic Levels by MPO") +
  labs(caption = paste0("Source: National Transit Database July 2022 Raw Release")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Transit Recovery is in the middle of the pack",
                                  p.chart = transit.mpo.chart)

# Transit by Mde ----------------------------------------------------
tbl <- transit.ytd %>%
  filter(geography=="Region" & variable!="Total Transit" & concept=="Transit Boardings") %>%
  filter(year >=2019) %>%
  mutate(year = as.character(year)) %>%
  mutate(str_wrap(variable, width=16))

transit.mode.chart <- create_facet_bar_chart(t=tbl, w.x="year", w.y="estimate", f="variable", g="variable",
                                            est.type = "number", 
                                            w.title = "Year to Date Transit Boardings",
                                            w.color = "psrc_blues", w.facet = 3) +
  labs(caption = paste0("Source: National Transit Database July 2022 Raw Release")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text.x = element_text(size=24), 
        axis.text.y = element_text(size=24),
        legend.position = "none",
        strip.text = element_text(size = 28))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Recovery has varied by Transit mode",
                                  p.chart = transit.mode.chart)


# Question Slide ------------------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Questions: chelmann@psrc.org",
                                  p.img=('X:/DSA/shiny-uploads/images/rhs-slide-2400x800.jpg'))

# Write Out Slides to Disc ------------------------------------------------
print(psrc_pres, target = "puget-sound-trends-executiveboard-sept-2022.pptx") 



