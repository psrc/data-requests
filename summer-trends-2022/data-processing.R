library(tidyverse)
library(here)
library(data.table)
library(lubridate)
library(psrccensus)
library(psrcplot)
library(psrctrends)

install_psrc_fonts()

# Passenger Screenings ----------------------------------------------------------
seatac.airport <- fread(here('data','SEA activity measures by week - SEA measures.csv')) %>%
  select(1:5) %>%
  slice(3:n()) %>%
  setnames(c("Week","2022","2021","2020","2019")) %>%
  mutate(Week = gsub("Week ","",Week)) %>%
  mutate(Week  =as.integer(Week)) %>%
  pivot_longer(cols=c("2022","2021","2020","2019"), names_to = "year", values_to ="passenger_screenings") %>%
  mutate(day = (ymd( "2022-01-01" ) + weeks(Week-1))) %>%
  mutate(passenger_screenings = gsub(",","",passenger_screenings)) %>%
  mutate(passenger_screenings = as.integer(passenger_screenings)) %>%
  mutate(year =as.character(year)) %>%
  drop_na()

seatac.chart <- create.line.chart(t=seatac.airport, w.x='day', w.y='passenger_screenings', w.g='year', est.type="number", 
                                  w.title="Weekly Passsenger Screenings: 2019 to 2022", d.form = "%B", w.lwidth=3)

# Airport Freight and Operations ----------------------------------------------------------
airport.operations <- process_sea_operations_data(c.yr=2022, c.mo=6)

# Airport Passengers
tbl <- airport.operations %>% filter(variable == "PASSENGER GRAND TOTAL")
seatac.passengers.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                  w.title="Monthly Passsenger Enplanements: 2019 to 2022", d.form = "%B", w.lwidth=3) 
rm(tbl)

# Air Cargo
tbl <- airport.operations %>% filter(variable == "CARGO GRAND TOTAL")
seatac.aircargo.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                             w.title="Monthly Air Cargo: 2019 to 2022", d.form = "%B", w.lwidth=3) 
rm(tbl)

# Air Operations
tbl <- airport.operations %>% filter(variable == "OPERATIONS GRAND TOTAL")
seatac.operations.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                           w.title="Monthly Air Operations: 2019 to 2022", d.form = "%B", w.lwidth=3)
rm(tbl)

# Employment Trends -------------------------------------------------------
qcew.jobs <- process_qcew_monthly_msa(c.yr=2022, c.mo=6)
tbl <- qcew.jobs %>% filter(variable=="Total Nonfarm") %>% filter(geography %in% c("Region", "Washington State")) %>% filter(year(data_day)>=2019)
total.jobs.chart <- create.line.chart(t=tbl, w.x='data_day', w.y='estimate', w.g='geography', w.title="Total Non-Farm Jobs: 2019 to 2022",
                                      d.form="%b-%Y", w.lwidth=3, est.type="number") 
rm(tbl)

# OFM Population Data 1990 to Present -----------------------------------------------------
ofm.pop <- get_ofm_intercensal_population()

# Annual Regional Population Change
tbl <- ofm.pop %>% 
  filter(Jurisdiction=="Region") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

population.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                             w.color="psrc_light",
                             w.title="Annual Population Change: 2013 to 2022",
                             w.interactive="no",
                             est.type = "number")

# Incorporated Area Population Change -------------------------------------
unincorp <- ofm.pop %>% 
  filter(Filter%in%c(2) & str_detect(Jurisdiction, "Region")) %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

incorp <- ofm.pop %>% 
  filter(Filter%in%c(3) & str_detect(Jurisdiction, "Region")) %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

tbl <- bind_rows(unincorp,incorp)

incorporated.population.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Delta", f="Jurisdiction", 
                                            w.color="psrc_pairs",
                                            w.title="Annual Population Change: 2013 to 2022",
                                            w.interactive="no",
                                            est.type = "number",
                                            w.pos="stack")

# Regional Geography Population Change ------------------------------------
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

# Growth in 2022
tbl <- ofm.pop.rgeo %>% filter(Year=="2022") %>% mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT"))
pop.rgeo.chart.22 <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", w.title = "Population Growth: 2022", est.type = "number") 

# Growth in the last 10 years
tbl <- ofm.pop.rgeo %>% 
  filter(Year>="2013") %>% 
  mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT")) %>%
  select(-Year) %>%
  group_by(`Regional Geography`) %>%
  summarize_all(sum) %>%
  as_tibble()

pop.rgeo.chart.ten <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", w.title = "Population Growth: 2013 to 2022", est.type = "number")

# OFM Housing Unit Data 1990 to Present -----------------------------------------------------
ofm.housing <- get_ofm_postcensal_housing()

# Annual Regional Housing Unit Change
tbl <- ofm.housing %>% 
  filter(Jurisdiction=="Region" & Variable == "Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))
  
housing.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                                            w.color="psrc_dark",
                                            w.title="Annual Housing Unit Change: 2013 to 2022",
                                            w.interactive="no",
                                            est.type = "number") 

rm(tbl)

# Incorporated Area Housing Change -------------------------------------
unincorp <- ofm.housing %>% 
  filter(Filter%in%c(2) & str_detect(Jurisdiction, "Region") & Variable=="Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

incorp <- ofm.housing %>% 
  filter(Filter%in%c(3) & str_detect(Jurisdiction, "Region") & Variable=="Total Housing Units") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

tbl <- bind_rows(unincorp,incorp)

incorporated.housing.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Delta", f="Jurisdiction", 
                                                         w.color="psrc_pairs",
                                                         w.title="Annual Housing Change: 2013 to 2022",
                                                         w.interactive="no",
                                                         est.type = "number",
                                                         w.pos="stack")

rm(tbl)

# Regional Housing Change by Regional Geography ---------------------------
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

# Growth in 2022
tbl <- ofm.housing.rgeo %>% filter(Year=="2022") %>% mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT"))
housing.rgeo.chart.22 <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", w.title = "Housing Unit Growth: 2022", est.type = "number")
rm(tbl)

# Growth in the last 10 years
tbl <- ofm.housing.rgeo %>% 
  filter(Year>="2013") %>% 
  mutate(`Regional Geography` = str_replace(`Regional Geography`, "High Capacity Transit Community", "HCT")) %>%
  select(-Year) %>%
  group_by(`Regional Geography`) %>%
  summarize_all(sum) %>%
  as_tibble()
  
housing.rgeo.chart.ten <- create_treemap_chart(t=tbl, w.area = "Delta", w.fill = "Regional Geography", w.title = "Housing Unit Growth: 2013 to 2022", est.type = "number")

rm(tbl)

# OFM Housing Unit Data by Type 1990 to Present -----------------------------------------------------

# Annual Housing Type Change for Region
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

housing.type.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                                                         w.title="Annual Population Change: 2013 to 2022",
                                                         w.interactive="no",
                                                         est.type = "number",
                                                         w.pos="stack")

rm(sf,mf,mh,tbl)

# OFM Components of Population Change 1960 to Present -----------------------------------------------------
ofm.pop.chg <- get_ofm_postcensal_pop_change()
tbl <- ofm.pop.chg %>% filter(Jurisdiction=="Region" & Year >= "2013")

population.type.change.chart <- create_bar_chart(t=tbl, w.x="Year", w.y="Estimate", f="Variable", 
                                              w.title="Annual Components of Population Change: 2013 to 2022",
                                              w.interactive="no",
                                              est.type = "number",
                                              w.pos="stack")

rm(tbl)

