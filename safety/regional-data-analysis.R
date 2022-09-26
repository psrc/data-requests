library(tidyverse)
library(tidycensus)
library(psrcplot)
library(psrcslides)
library(officer)

install_psrc_fonts()
psrc_pres = read_pptx(system.file('extdata', 'psrc-trends-template.pptx', package='psrcslides'))

census.yr <- 2019
fars.yr <- 2020

metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

# MPO Cenus Data ----------------------------------------------------------------
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

# MPO Accident Data -------------------------------------------------------
fars_data <- NULL
for (y in seq(fars.yr-4,fars.yr,by=1)) {
  
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

mpo_data <- mpo_county_data %>%
  select(-MSA_FIPS, -MSA_NAME, -COUNTY_FIPS, -COUNTY_NAME, -STATE_FIPS, -STATE_NAME, -STATE_LONG_NAME,-GEOID) %>%
  group_by(MPO_AREA, MPO_FIPS, MPO_NAME) %>%
  summarise(across(.fns = sum)) %>%
  mutate(People_of_Color_Share = People_of_Color/Population) %>%
  mutate(NonSOV_Share = NonSOV_Commute_Trips/Commute_Trips) %>%
  mutate(Fatal_Collision_Rate = ((Fatal_Collisions_5yr/5)/Population)*100000)

# MPO Historically Disadvantaged Communities ------------------------------
mpo_county_names <- mpo %>% 
  select(COUNTY_NAME,STATE_LONG_NAME) %>% 
  mutate(COUNTY = paste0(COUNTY_NAME,",",STATE_LONG_NAME)) %>%
  select(COUNTY) %>%
  distinct() %>% 
  pull()

mpo_names <- mpo %>%
  select(MPO_AREA, MPO_FIPS, MPO_NAME, COUNTY_NAME, STATE_LONG_NAME) %>%
  rename(State=STATE_LONG_NAME, County=COUNTY_NAME)

tract_hdc <- read_csv("X:/DSA/shiny-uploads/data/RAISE_Persistent_Poverty.csv") %>%
  mutate(temp = paste0(County,",",State)) %>%
  filter(temp %in% mpo_county_names) %>%
  select(-temp)

tract_population = NULL
for (st in states) {
  
  c <- mpo %>% filter(STATE_FIPS %in% st) %>% select(COUNTY_FIPS) %>% pull()
  
  d <- get_acs(geography = "tract", state=st, county=c, variables = c("B01001_001"), year = census.yr, survey = "acs5") %>%
    separate(col=NAME, sep=", ", into=c("Tract", "County", "State")) %>%
    mutate(County = gsub(" County","",County)) %>%
    rename(population=estimate) %>%
    select(-variable, -moe)
  
  ifelse(is.null(tract_population), tract_population <- d, tract_population <- bind_rows(tract_population,d))
  
  rm(d, c)

}

mpo_hdc <- left_join(tract_hdc, tract_population, by=c("Tract","County","State"))
mpo_hdc <- left_join(mpo_hdc, mpo_names, by=c("County","State"))

mpo_hdc <- mpo_hdc %>%
  select(MPO_FIPS, HDC_TRACT, population) %>%
  group_by(MPO_FIPS, HDC_TRACT) %>%
  summarise(population=sum(population)) %>%
  as_tibble %>%
  filter(HDC_TRACT=="Yes") %>%
  rename(HDC_Population=population) %>%
  select(-HDC_TRACT)

mpo_data <- left_join(mpo_data, mpo_hdc, by=c("MPO_FIPS")) %>%
  mutate(HDC_Share = HDC_Population/Population)

# Population Slide by MPO --------------------------------------------------
tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(Population)

mpo_order <- tbl %>% select(MPO_FIPS) %>% pull()
tbl <- tbl %>% mutate(MPO_FIPS = factor(x=MPO_FIPS, levels=mpo_order))

psrc.pop <- mpo_data %>% filter(MPO_FIPS=="PSRC") %>% select(Population) %>% pull()

total.pop.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_FIPS", w.y="Population",
                                        f="plot_id", w.color = "psrc_light",
                                        w.title="Total Population by MPO: 2019",
                                        est.type = "number") +
  labs(caption = paste0("Source: 2015-2019 American Community Survey 5yr Data Table B01001")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  annotate("text", x = 12, y = 5000000, label = paste0("PSRC: ", prettyNum(psrc.pop, big.mark = ",")), size = 8, family="Poppins")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Total Population by Regional Entity",
                                  p.chart = total.pop.mpo.chart)

# Population in HDC Slide by MPO --------------------------------------------------
tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(HDC_Share)

mpo_order <- tbl %>% select(MPO_FIPS) %>% pull()
tbl <- tbl %>% mutate(MPO_FIPS = factor(x=MPO_FIPS, levels=mpo_order))

psrc.hdc <- mpo_data %>% filter(MPO_FIPS=="PSRC") %>% select(HDC_Share) %>% pull()

hdc.pop.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_FIPS", w.y="HDC_Share",
                                        f="plot_id", w.color = "psrc_light",
                                        w.title="Share of Population in Historically Underserved Communites Tracts by MPO: 2019") +
  labs(caption = paste0("Source: https://datahub.transportation.gov/stories/s/tsyd-k6ij")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  annotate("text", x = 17, y = 0.36, label = paste0("PSRC: ", prettyNum(round(psrc.hdc*100,1), big.mark = ","),"%"), size = 8, family="Poppins")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Share of Population in Historically Underserved Communites",
                                  p.chart = hdc.pop.mpo.chart)

# Fatal Collision Rate Slide by MPO --------------------------------------------------
tbl <- mpo_data %>% 
  mutate(plot_id = case_when(
    MPO_AREA == "Seattle" ~ "PSRC",
    MPO_AREA %in% metros ~ "Comparable Metros")) %>%
  mutate(plot_id=replace_na(plot_id,"Other")) %>%
  arrange(Fatal_Collision_Rate)

mpo_order <- tbl %>% select(MPO_FIPS) %>% pull()
tbl <- tbl %>% mutate(MPO_FIPS = factor(x=MPO_FIPS, levels=mpo_order))

psrc.fatal.rate <- mpo_data %>% filter(MPO_FIPS=="PSRC") %>% select(Fatal_Collision_Rate) %>% pull()

fatal.collisions.rate.mpo.chart <- create_bar_chart(t=tbl, w.x="MPO_FIPS", w.y="Fatal_Collision_Rate",
                                      f="plot_id", w.color = "psrc_light",
                                      w.title="Average Annual Fatalities per 100,000 people by MPO: 2020",
                                      est.type = "number") +
  labs(caption = paste0("Source:2015-2019 American Community Survey 5yr Data & FARS National Data 2016-2020")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24),
        legend.position = "none") +
  annotate("text", x = 6, y = 5.75, label = paste0("PSRC: ", prettyNum(round(psrc.fatal.rate,2), big.mark = ",")), size = 8, family="Poppins")

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Average Annual Motor-Vehicle-Involved Roadway Fatalities",
                                  p.chart = fatal.collisions.rate.mpo.chart)

# Write Out Slides to Disc ------------------------------------------------
print(psrc_pres, target = "mpo-comparisons-ss4a.pptx") 
