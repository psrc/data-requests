library(tidyverse)
library(tidycensus)

mpo <- read_csv("X:/DSA/shiny-uploads/data/regional-councils-counties.csv") %>% 
  mutate(COUNTY_FIPS=str_pad(COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
  mutate(STATE_FIPS=str_pad(STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
  mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS))

states <- mpo %>% select(STATE_FIPS) %>% distinct() %>% pull()
state_abb <- mpo %>% select(STATE_NAME) %>% distinct() %>% pull()
counties <- mpo %>% select(GEOID) %>% distinct() %>% pull()

st_lookup <- mpo %>% select(STATE_NAME,STATE_LONG_NAME) %>% distinct()

cities <- read_csv("X:/DSA/shiny-uploads/data/SUB-EST2020_ALL.csv") %>%
  mutate(GEOID = paste0(STATE,COUNTY)) %>%
  filter(STATE %in% states) %>%
  filter(GEOID %in% counties) %>%
  filter(PLACE != "00000") %>%
  filter(!(FUNCSTAT %in% c("F","S"))) %>%
  mutate(NAME = gsub(" city", "", NAME)) %>%
  mutate(NAME = gsub(" village", "", NAME)) %>%
  mutate(NAME = gsub(" borough", "", NAME)) %>%
  mutate(NAME = gsub(" town", "", NAME)) %>%
  mutate(NAME = gsub(" \\(pt.\\)", "", NAME)) %>%
  mutate(Place_Name = paste0(NAME,", ",STNAME)) %>%
  select(Place_Name) %>%
  pull() %>%
  unique()

agencies <- read_csv("X:/DSA/shiny-uploads/data/transit-agency.csv") %>%
  mutate(CITY = str_to_title(CITY)) %>%
  filter(STATE %in% state_abb)

agencies <- left_join(agencies, st_lookup, by=c("STATE"="STATE_NAME")) %>%
  mutate(Place_Name = paste0(CITY,", ",STATE_LONG_NAME)) %>%
  filter(Place_Name %in% cities)


