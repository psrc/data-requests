library(tidyverse)
library(here)
library(data.table)
library(lubridate)
library(psrccensus)
library(tidycensus)
library(psrcplot)
library(psrctrends)
library(officer)
library(sf)

install_psrc_fonts()

# Basic Inputs ------------------------------------------------------------
presentation.title <- "Puget Sound Trends"
committee.name <- "Growth Management Policy Board"
presentation.date <- "September 2022"

today <- Sys.Date()
current.yr <- year(today)
full.yr <- current.yr-1

# Race
white.alone <- c("B03002_003")
people.of.color <- c("B03002_004","B03002_005","B03002_006","B03002_007","B03002_008","B03002_009","B03002_012")
hispanc <- c("B03002_012")
msa.pop.limit <- 1000000

# Modes
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

wgs84 <- 4326
spn <- 2285 
hct.buffer <- 0.5

my_pres <- read_pptx("trends-psrc-template-revised.pptx")

# Create Title Slide ------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Title Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/bellevuelakewashington_3.jpg")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value=presentation.title, 
                   location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x=my_pres, 
                   value=committee.name, 
                   location = ph_location_type(type = "subTitle"))
my_pres <- ph_with(x=my_pres, 
                   value=presentation.date, 
                   location = ph_location_type(type = "dt"))

# Population Section Slide Transition------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Transition Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/slide-uw-equity_0.png")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value="Population", 
                   location = ph_location_type(type = "title"))

# Annual Population Growth -------------------------------------------------------
ofm.pop <- get_ofm_intercensal_population()

tbl <- ofm.pop %>% 
  filter(Jurisdiction=="Region") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-2)) %>%
  filter(Year >=2013) %>%
  mutate(Year = as.character(Year))

pop.growth <- tbl %>% filter(Year=="2022") %>% select(Delta) %>% pull()
pop.ovr.50k <- tbl %>% filter(Delta>=50000) %>% select(Delta) %>% pull() %>% length()

population.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Variable", 
                                            w.color="GnPr",
                                            w.title="Annual Population Change: 2013 to 2022",
                                            w.sub.title="Source: Office of Financial Management April 1st Population Estimates",
                                            w.interactive="no",
                                            est.type = "number") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Despite a Global Pandemic, the region continues to grow", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The region added ", 
                                prettyNum(pop.growth, big.mark = ","),
                                " people between 2021 and 2022. In ",
                                pop.ovr.50k, 
                                " out of the last 10 years the region has grown by more than 50,000 people."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=population.change.chart, 
                   location = ph_location_type(type = "pic"))

# Population Growth Compared to Vision 2050 -------------------------------
observed.pop <- ofm.pop %>% 
  filter(Jurisdiction=="Region" & Year>=2010) %>% 
  mutate(Variable="Observed") %>% 
  select(Year, Estimate, Variable)

observed.forecast.pop <- ofm.pop %>% 
  filter(Jurisdiction=="Region" & Year>=2010 & Year<=2018) %>% 
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
                                  w.sub.title="Source: Office of Financial Management & PSRC Regional Macroeconomic Forecast",
                                  x.type= "Continuous",
                                  w.lwidth = 3,
                                  w.breaks = c("2010","2015","2020","2025"),
                                  w.color = "DkPrLtPr")+
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="So far, so good", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2018 and 2022, population growth is in-line with VISION 2050. In VISION 2050 forecasts, the population in 2022 was forecasted to be ", 
                                prettyNum(round(pop.vision.2022,-3), big.mark = ","),
                                ". The observed population in 2022 was  ",
                                prettyNum(round(pop.observed.2022,-3), big.mark = ","), 
                                ", a difference of ",
                                prettyNum(round(pop.observed.2022.delta,-3), big.mark = ","),
                                " people."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=pop.growth.vision.chart, 
                   location = ph_location_type(type = "pic"))

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
                                                 w.sub.title="Source: Office of Financial Management April 1st Population Estimates",
                                                 w.interactive="no",
                                                 est.type = "number",
                                                 w.pos="stack",
                                                 w.color = "OrPr") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Migration continues to drive population growth", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(migration.2022,-2), big.mark = ","),
                                " people migrated to the region between 2021 and 2022, ",
                                prettyNum(round(migration.share.2022*100,0), big.mark = ","),
                                "% of the total growth. In all,migration has accounted for ",
                                prettyNum(round(migration.share.ten*100,0), big.mark = ","),
                                "% of the total growth in the past 10 years"), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=population.type.change.chart, 
                   location = ph_location_type(type = "pic"))

# Population by Race for Region------------------------------------------------------
pop.by.race <- get_acs_recs(geography = 'county',table.names = c('B03002'), years=c(2010,2015,2020), acs.type = 'acs5')

tbl <- pop.by.race %>%
  filter(variable %in% c(people.of.color, white.alone)) %>%
  mutate(POC = case_when(
    variable %in% people.of.color ~ "Person of Color",
    variable %in%white.alone ~ "Non-Hispanic White")) %>%
  select(name, POC, year, estimate) %>%
  group_by(name, POC, year) %>%
  summarise(estimate=sum(estimate))

tot <- pop.by.race %>%
  filter(variable %in% c("B03002_001")) %>%
  select(name, year, estimate) %>%
  rename(total=estimate)

tbl <- left_join(tbl, tot, by=c("name","year")) %>%
  mutate(share=estimate/total, year=as.character(year))

poc.2010 <- tbl %>% filter(year=="2010" & POC=="Person of Color" & name=="Region") %>% select(share) %>% pull()
poc.2015 <- tbl %>% filter(year=="2015" & POC=="Person of Color" & name=="Region") %>% select(share) %>% pull()
poc.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="Region") %>% select(share) %>% pull()

poc.king.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="King County") %>% select(share) %>% pull()
poc.kitsap.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="Kitsap County") %>% select(share) %>% pull()
poc.pierce.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="Pierce County") %>% select(share) %>% pull()
poc.snohomish.2020 <- tbl %>% filter(year=="2020" & POC=="Person of Color" & name=="Snohomish County") %>% select(share) %>% pull()

pop.by.race.chart <- create_column_chart(t=tbl%>%filter(name=="Region"), w.x="year", w.y="share", w.pos = "stack",f="POC", 
                                      w.color = "GnOr",
                                      w.title="People of Color: 2010, 2015 and 2020",
                                      w.sub.title="Source: ACS 5-yr Data Table B03002") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

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

my_pres <- add_slide(my_pres, layout = "Title with Bullets, Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="The region continues to diversify", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In 2020, People of Color accounted for approximately ",
                                prettyNum(round(poc.2020*100,0), big.mark = ","),
                                "% of the total population in the region. This was an increase from ",
                                prettyNum(round(poc.2010*100,0), big.mark = ","),
                                "% in 2010."),
                   location = ph_location_label(ph_label = "Text Placeholder 4"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In 2020 there were approximately ", 
                                prettyNum(round(indigenous.tot,-2), big.mark = ","),
                                " Indigenous people living in the Puget Sound region, about ",
                                prettyNum(round(indigenous.shr*100,1), big.mark = ","),
                                "% of the total population."),
                   location = ph_location_label(ph_label = "Text Placeholder 5"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(asian.shr*100,1), big.mark = ","),
                                "% of the total population in the region were of Asian descent."),
                   location = ph_location_label(ph_label = "Text Placeholder 8"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("People of Color accounted for approximately ", 
                                prettyNum(round(poc.king.2020*100,0), big.mark = ","),
                                "% of the total population in King County, the largest share of any county."), 
                   location = ph_location_label(ph_label = "Text Placeholder 10"))
my_pres <- ph_with(x=my_pres, 
                   value=pop.by.race.chart, 
                   location = ph_location_type(type = "pic"))

# Population by Race for Metro Areas------------------------------------------------------
pop.by.race.msa <- get_acs(geography="metropolitan statistical area/micropolitan statistical area", year=2020, survey='acs5', table='B03002') 

tbl <- pop.by.race.msa %>%
  filter(variable %in% c(people.of.color, white.alone)) %>%
  mutate(NAME = gsub("Bremerton-Silverdale-Port Orchard, WA Metro Area", "Seattle-Tacoma-Bellevue, WA Metro Area", NAME)) %>%
  mutate(POC = case_when(
    variable %in% people.of.color ~ "Person of Color",
    variable %in%white.alone ~ "Non-Hispanic White")) %>%
  select(NAME, POC, estimate) %>%
  group_by(NAME, POC) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

tot <- pop.by.race.msa %>%
  filter(variable %in% c("B03002_001")) %>%
  mutate(NAME = gsub("Bremerton-Silverdale-Port Orchard, WA Metro Area", "Seattle-Tacoma-Bellevue, WA Metro Area", NAME)) %>%
  select(NAME, estimate) %>%
  rename(total=estimate) %>%
  group_by(NAME) %>%
  summarise(total=sum(total)) %>%
  as_tibble()

tbl <- left_join(tbl, tot, by=c("NAME")) %>%
  mutate(share=estimate/total) %>%
  filter(total >= msa.pop.limit & POC=="Person of Color" & NAME!="San Juan-Bayamón-Caguas, PR Metro Area") %>%
  arrange(desc(share))

tbl.lvls <- tbl %>% select(NAME) %>% pull() 

tbl <- tbl %>% 
  mutate(NAME=factor(NAME, levels=tbl.lvls)) %>%
  mutate(plot_id = case_when(
    total >= 1000000 & total < 2000000 ~ "1-2M",
    total >= 2000000 & total < 3000000 ~ "2-3M",
    total >= 3000000 & total < 4000000 ~ "3-4M",
    total >= 4000000 & total < 5000000 ~ "4-5M",
    total >= 5000000  ~ "5M+")) %>%
  mutate(plot_id = case_when(
    NAME=="Seattle-Tacoma-Bellevue, WA Metro Area" ~ "PSRC",
    NAME!="Seattle-Tacoma-Bellevue, WA Metro Area" ~ plot_id)) %>%
  mutate(Rank = order(share, decreasing = TRUE))

psrc.rank <- tbl %>% filter(NAME=="Seattle-Tacoma-Bellevue, WA Metro Area") %>% select(Rank) %>% pull()
total.metros <- tbl %>% select(NAME) %>% pull() %>% length()
  
pop.by.race.msa.chart <- create_column_chart(t=tbl, w.x="NAME", w.y="share", f="plot_id", 
                                          w.color = "psrc_dark",
                                          w.title="People of Color by Metro Area with at least 1 Million People: 2020",
                                          w.sub.title="Source: ACS 5-yr Data Table B03002") +
  theme(plot.title = element_text(size=28), axis.text.y = element_text(size=24), axis.text.x = element_blank(), legend.text = element_text(size=24))
  

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="The region is less diverse than many Metro Areas", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The region ranks ", 
                                prettyNum((psrc.rank), big.mark = ","),
                                " out of ",
                                prettyNum((total.metros), big.mark = ","), 
                                " metro regions over 1 million people in size for share of people of color."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=pop.by.race.msa.chart, 
                   location = ph_location_type(type = "pic"))

# Incorporated Population Growth -------------------------------------------------------
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

inc.pop.growth <- tbl %>% filter(Jurisdiction=="Incorporated Region") %>% select(Delta) %>% pull() %>% sum()
uninc.pop.growth <- tbl %>% filter(Jurisdiction=="Unincorporated Region") %>% select(Delta) %>% pull() %>% sum()
per.inc.pop.growth = inc.pop.growth / (inc.pop.growth+uninc.pop.growth)

incorporated.population.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Jurisdiction", 
                                                         w.color="DkPrLtPr",
                                                         w.title="Annual Population Change: 2013 to 2022",
                                                         w.sub.title="Source: Office of Financial Management April 1st Population Estimates",
                                                         w.interactive="no",
                                                         est.type = "number",
                                                         w.pos="stack") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="The vast majority of growth is in cities", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In the past ten years the region grew by ", 
                                prettyNum((inc.pop.growth+uninc.pop.growth), big.mark = ","),
                                " people. Approximately ",
                                prettyNum(round(per.inc.pop.growth*100,0), big.mark = ","), 
                                "% of that growth was in incorporated places."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=incorporated.population.change.chart, 
                   location = ph_location_type(type = "pic"))

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
                                          w.sub.title="Source: Office of Financial Management April 1st Population Estimates",
                                          est.type = "number") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24)) 

my_pres <- add_slide(my_pres, layout = "Title with Bullets, Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Big Cities are not dying", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2021 and 2022, ",
                                prettyNum(round(metro.pop.growth.share.2022*100,0), big.mark = ","),
                                "% of the total population growth in the region was in Metropolitan Cities and more than ",
                                prettyNum(round(top3.pop.growth.share.2022*100,0), big.mark = ","),
                                "% of the growth was in Metropolitan, Core and High Capacity Transit cities."),
                   location = ph_location_label(ph_label = "Text Placeholder 4"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The City of Seattle added ", 
                                prettyNum(round(seattle.pop.growth.2022,-2), big.mark = ","),
                                " people in 2022, about ",
                                prettyNum(round(seattle.pop.growth.share.2022*100,0), big.mark = ","),
                                "% of the total regional population growth last year."),
                   location = ph_location_label(ph_label = "Text Placeholder 5"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(metro.pop.growth.ten,-2), big.mark = ","),
                                " people moved into Metropolitan Cities in the last ten years, ",
                                prettyNum(round(metro.pop.growth.share.ten*100,0), big.mark = ","),
                                "% of the total regional population growth."),
                   location = ph_location_label(ph_label = "Text Placeholder 8"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The five fastest growing cities in the past ten years were: ", 
                                paste(top.five.cities,collapse = ", "),
                                ". These five citites added more than ",
                                prettyNum(round(top.five.cities.growth,-2), big.mark = ","),
                                " people in the last ten years."), 
                   location = ph_location_label(ph_label = "Text Placeholder 10"))
my_pres <- ph_with(x=my_pres, 
                   value=pop.rgeo.chart.22, 
                   location = ph_location_type(type = "pic"))

# Population Growth near HCT ----------------------------------------------

# Blocks with HCT Stops
hct.stops <- st_read(here('data', 'hct_stops_2050.shp')) %>% 
  st_transform(spn) %>%
  st_buffer(dist=hct.buffer*5280) %>%
  st_union() %>%
  st_sf() %>%
  mutate(HCT_Buffer = "Yes")

psrc.blocks <- st_read(here('data', 'block2010.shp'))%>% 
  st_transform(spn) %>%
  select(GEOID10)

psrc.hct.blocks <- st_intersection(psrc.blocks, hct.stops)

blocks <- psrc.blocks %>% st_drop_geometry()
hct.blocks <- psrc.hct.blocks %>% st_drop_geometry()

final.blocks <- left_join(blocks, hct.blocks, by=c("GEOID10")) %>% mutate(HCT_Buffer = replace_na(HCT_Buffer,"No"))

# Load Population and Housing Data by Census Block -------------------------
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
                                               w.sub.title="Source: Office of Financial Management Small Area Estimates Program",
                                               x.type= "Continuous",
                                               est.type = "percent",
                                               w.lwidth = 3,
                                               w.breaks = c("2013","2014","2015","2016","2017","2018","2019","2020"),
                                               w.color = "BkPr") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24)) +
  scale_y_continuous(labels = scales::label_percent(), limits=c(0,1))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Over half of population growth is near High Capacity Transit", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2018 and 2020, approximately ", 
                                prettyNum(round(hct.growth.share.since.2018*100,0), big.mark = ","),
                                "% of population growth has occurred within a half mile of current or planned ",
                                "high capacity transit."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=pop.growth.near.hct.chart, 
                   location = ph_location_type(type = "pic"))


# Housing Section Slide Transition------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Transition Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/housing.jpg")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value="Housing", 
                   location = ph_location_type(type = "title"))

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
                                         w.sub.title="Source: Office of Financial Management",
                                         w.interactive="no",
                                         est.type = "number") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

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

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Housing construction is robust", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(region.housing.delta.2022,-2), big.mark = ","),
                                " new housing units were finsihed in 2022. This ranks #",
                                region.housing.delta.2022.rank,
                                " out of ",
                                housing.since.1992,
                                " in total annual production since 1992."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=housing.change.chart, 
                   location = ph_location_type(type = "pic"))

# Housing Compared to VISION 2050 -----------------------------------------
observed.housing <- ofm.housing %>% 
  filter(Jurisdiction=="Region" & Variable=="Total Housing Units") %>% 
  filter(Year >=2010) %>%
  select(Year, Variable, Estimate) %>% 
  mutate(Variable="Observed")

observed.forecast <- observed.housing %>% 
  filter(Year>=2010 & Year<2018) %>%
  mutate(Variable="Forecast")

future.housing <- as_tibble(fread(here('data','regional-housing.csv'))) %>% 
  filter(Year>=2018) %>%
  select(Year, Forecast) %>%
  rename(Estimate=Forecast) %>%
  mutate(Variable="Forecast")

vision.housing <- bind_rows(list(observed.housing, observed.forecast, future.housing)) %>%
  mutate(Year=as.character(Year))

housing.growth.vision.chart <- create.line.chart(t=vision.housing %>% filter(Year<=2025), w.x='Year', w.y='Estimate', w.g='Variable', est.type="number", 
                                             w.title="Annual Housing Unit Forecast: 2010 to 2025",
                                             w.sub.title="Source: Office of Financial Management & PSRC Regional Macroeconomic Forecast",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025"),
                                             w.color = "DkPrLtPr")+
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Housing production is close", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2018 and 2022, population growth is in-line with VISION 2050. In VISION 2050 forecasts, the population in 2022 was forecasted to be ", 
                                prettyNum(round(pop.vision.2022,-3), big.mark = ","),
                                ". The observed population in 2022 was  ",
                                prettyNum(round(pop.observed.2022,-3), big.mark = ","), 
                                ", a difference of ",
                                prettyNum(round(pop.observed.2022.delta,-3), big.mark = ","),
                                " people."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=housing.growth.vision.chart, 
                   location = ph_location_type(type = "pic"))

# Incorporated Area Housing Growth ----------------------------------------
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

incorp.housing.total <- tbl %>% filter(Jurisdiction=="Incorporated Region") %>% select(Delta) %>% pull() %>% sum()
unincorp.housing.total <- tbl %>% filter(Jurisdiction=="Unincorporated Region") %>% select(Delta) %>% pull() %>% sum()
incorp.housing.share <- incorp.housing.total / (incorp.housing.total+unincorp.housing.total)

incorporated.housing.change.chart <- create_column_chart(t=tbl, w.x="Year", w.y="Delta", f="Jurisdiction", 
                                                      w.color="GnOr",
                                                      w.title="Annual Housing Change: 2013 to 2022",
                                                      w.sub.title="Source: Office of Financial Management",
                                                      w.interactive="no",
                                                      est.type = "number",
                                                      w.pos="stack") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Most new housing is in cities", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(incorp.housing.total,-2), big.mark = ","),
                                " new housing units were constructed in Incorporated Places since 2013, ",
                                prettyNum(round(incorp.housing.share*100,0), big.mark = ","),
                                "% of the total housing production over the past 10 years."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=incorporated.housing.change.chart, 
                   location = ph_location_type(type = "pic"))

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
                                              w.sub.title="Source: Office of Financial Management",
                                              w.interactive="no",
                                              est.type = "number",
                                              w.pos="stack",
                                              w.color = "OrPr") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Most new housing is multi-family", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(mf.housing.delta,-2), big.mark = ","),
                                " new multi-family housing units were constructed since 2013, ",
                                prettyNum(round(mf.housing.share*100,0), big.mark = ","),
                                "% of the total housing production over the past 10 years."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=housing.type.change.chart, 
                   location = ph_location_type(type = "pic"))

rm(sf,mf,mh,tbl)

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
                                              w.sub.title="Source: Office of Financial Management",
                                              est.type = "number") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))
rm(tbl)

my_pres <- add_slide(my_pres, layout = "Title with Bullets, Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Big Cities are adding a large share of the housing", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2021 and 2022, ",
                                prettyNum(round(metro.housing.growth.share.2022*100,0), big.mark = ","),
                                "% of the total housing growth in the region was in Metropolitan Cities and more than ",
                                prettyNum(round(top3.housing.growth.share.2022*100,0), big.mark = ","),
                                "% of the growth was in Metropolitan, Core and High Capacity Transit cities."),
                   location = ph_location_label(ph_label = "Text Placeholder 4"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The City of Seattle added ", 
                                prettyNum(round(seattle.housing.growth.2022,-2), big.mark = ","),
                                " housing units in 2022, about ",
                                prettyNum(round(seattle.housing.growth.share.2022*100,0), big.mark = ","),
                                "% of the total regional housing unit growth last year."),
                   location = ph_location_label(ph_label = "Text Placeholder 5"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(metro.housing.growth.ten,-2), big.mark = ","),
                                " housing units were added to Metropolitan Cities in the last ten years, ",
                                prettyNum(round(metro.housing.growth.share.ten*100,0), big.mark = ","),
                                "% of the total regional housing unit growth."),
                   location = ph_location_label(ph_label = "Text Placeholder 8"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("The five cities that added the most housing in the past ten years were: ", 
                                paste(top.five.cities.housing,collapse = ", "),
                                ". These five citites added more than ",
                                prettyNum(round(top.five.cities.housing.growth,-2), big.mark = ","),
                                " housing units in the last ten years."), 
                   location = ph_location_label(ph_label = "Text Placeholder 10"))
my_pres <- ph_with(x=my_pres, 
                   value=housing.rgeo.chart.22, 
                   location = ph_location_type(type = "pic"))

# Rental Trends -----------------------------------------------------------
region.rentals <- as_tibble(fread(here('data','Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv'))) %>% 
  filter(RegionName == "Seattle, WA") %>% 
  select(-RegionID, -SizeRank) %>% 
  pivot_longer(cols=contains("-"), names_to="Year", values_to="Estimate") %>%
  mutate(Year = str_replace(Year, "X", "")) %>%
  mutate(Year = ym(Year)) %>%
  mutate(Variable="Median Monthly Rent") %>% 
  mutate(Mo=month(Year)) %>% 
  mutate(Yr=year(Year))

rent.max.year <- region.rentals %>% select(Yr) %>% pull %>% max()
rent.max.month <- region.rentals %>% filter(Yr==rent.max.year) %>% select(Mo) %>% pull() %>% max()
rent.2019 <- region.rentals %>% filter(Yr==2019 & Mo==rent.max.month) %>% select(Estimate) %>% pull()
rent.current <- region.rentals %>% filter(Yr==rent.max.year & Mo==rent.max.month) %>% select(Estimate) %>% pull()
rent.per.inc <- (rent.current/rent.2019)-1

rentals.chart <- create_column_chart(t=region.rentals, w.x="Year", w.y="Estimate", f="Variable", 
                                  w.title="Monthly Median Rent: 2014 to 2022",
                                  w.sub.title="Source: Zillow Rental Data at https://www.zillow.com/research/data/",
                                  w.interactive="no",
                                  est.type = "currency",
                                  w.color = "GnOr") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Despite robust construction, Rents are at an all time high", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In June of 2019, the median rent in the Seattle Metro area was $", 
                                prettyNum(round(rent.2019,-1), big.mark = ","),
                                " per month. In June of 2022, the average rent was $",
                                prettyNum(round(rent.current,-1), big.mark = ","),
                                " per month, a ",
                                prettyNum(round(rent.per.inc*100,0), big.mark = ","),
                                "% increase and an all-time high."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=rentals.chart, 
                   location = ph_location_type(type = "pic"))

# Housing Price Trends ----------------------------------------------------
region.housing.sales <- as_tibble(fread(here('data','home-sales.csv')))

median.price <- region.housing.sales %>% select(`Month of Period End`, `Median Sale Price`) %>% 
  mutate(`Month of Period End` = my(`Month of Period End`)) %>% 
  rename(Month=`Month of Period End`, Estimate=`Median Sale Price`) %>% 
  mutate(Variable="Median Sales Price") %>%
  mutate(Estimate = gsub("\\$","",Estimate)) %>%
  mutate(Estimate = gsub("K","",Estimate)) %>%
  mutate(Estimate = as.integer(Estimate)*1000) %>% 
  mutate(Mo=month(Month)) %>% 
  mutate(Yr=year(Month))

price.max.year <- median.price %>% select(Yr) %>% pull %>% max()
price.max.month <- median.price %>% filter(Yr==rent.max.year) %>% select(Mo) %>% pull() %>% max()
price.2019 <- median.price %>% filter(Yr==2019 & Mo==price.max.month) %>% select(Estimate) %>% pull()
price.current <- median.price %>% filter(Yr==price.max.year & Mo==price.max.month) %>% select(Estimate) %>% pull()
price.per.inc <- (price.current/price.2019)-1

price.chart <- create_column_chart(t=median.price %>% filter(Yr>=2013), 
                                w.x="Month", w.y="Estimate", f="Variable", 
                                w.title="Median Sales Price: 2013 to 2022",
                                w.sub.title="Source: Redfin Housing Data at https://www.redfin.com/news/data-center/",
                                est.type = "currency",
                                w.color = "OrGn") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Home price increases are beginning to slow", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In June of 2019, the median sale price for a home in the Seattle Metro area was $", 
                                prettyNum(round(price.2019,-1), big.mark = ","),
                                ". In June of 2022, the average price was $",
                                prettyNum(round(price.current,-1), big.mark = ","),
                                " a ",
                                prettyNum(round(price.per.inc*100,0), big.mark = ","),
                                "% increase."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=price.chart, 
                   location = ph_location_type(type = "pic"))

# Homes for Sale ----------------------------------------------------------
homes.sold <- region.housing.sales %>% 
  select(`Month of Period End`, `Homes Sold`) %>% 
  mutate(`Month of Period End` = my(`Month of Period End`)) %>% 
  rename(Month=`Month of Period End`, Estimate=`Homes Sold`) %>% 
  mutate(Variable="Homes Sold") %>% 
  mutate(Mo=month(Month)) %>% 
  mutate(Yr=year(Month)) %>%
  mutate(Estimate = gsub(",","",Estimate)) %>%
  mutate(Estimate=as.integer(Estimate))

sales.max.year <- homes.sold %>% select(Yr) %>% pull %>% max()
sales.max.month <- homes.sold %>% filter(Yr==rent.max.year) %>% select(Mo) %>% pull() %>% max()
sales.2019 <- homes.sold %>% filter(Yr==2019 & Mo==sales.max.month) %>% select(Estimate) %>% pull()
sales.current <- homes.sold %>% filter(Yr==sales.max.year & Mo==sales.max.month) %>% select(Estimate) %>% pull()
sales.per.inc <- (sales.current/sales.2019)-1

sales.chart <- create_column_chart(t=homes.sold %>% filter(Yr>=2013), 
                                w.x="Month", w.y="Estimate", f="Variable", 
                                w.title="Monthly Homes Sold: 2013 to 2022",
                                w.sub.title="Source: Redfin Housing Data at https://www.redfin.com/news/data-center/",
                                est.type = "number",
                                w.color = "PrGn") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

homes.inventory <- region.housing.sales %>% 
  select(`Month of Period End`, Inventory) %>% 
  mutate(`Month of Period End` = my(`Month of Period End`)) %>% 
  rename(Month=`Month of Period End`, Estimate=Inventory) %>% 
  mutate(Variable="Home Inventory") %>% 
  mutate(Mo=month(Month)) %>% 
  mutate(Yr=year(Month)) %>%
  mutate(Estimate = gsub(",","",Estimate)) %>%
  mutate(Estimate=as.integer(Estimate))

inventory.max.year <- homes.inventory %>% select(Yr) %>% pull %>% max()
inventory.max.month <- homes.inventory %>% filter(Yr==rent.max.year) %>% select(Mo) %>% pull() %>% max()
inventory.2019 <- homes.inventory %>% filter(Yr==2019 & Mo==inventory.max.month) %>% select(Estimate) %>% pull()
inventory.current <- homes.inventory %>% filter(Yr==inventory.max.year & Mo==inventory.max.month) %>% select(Estimate) %>% pull()
inventory.per.inc <- (inventory.current/inventory.2019)-1

inventory.chart <- create_column_chart(t=homes.inventory %>% filter(Yr>=2013), 
                                w.x="Month", w.y="Estimate", f="Variable", 
                                w.title="Monthly Homes on the Market: 2013 to 2022",
                                w.sub.title="Source: Redfin Housing Data at https://www.redfin.com/news/data-center/",
                                est.type = "number",
                                w.color = "BkGn") + 
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Two Charts with Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Inventory has been driving cost", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In June of 2019, approximately ", 
                                prettyNum(round(inventory.2019,-1), big.mark = ","),
                                " homes were for sale in the Seattle Metro Area. In June of 2022, there were about ",
                                prettyNum(round(inventory.current,-1), big.mark = ","),
                                " homes for sale, a ",
                                prettyNum(round(inventory.per.inc*100,0), big.mark = ","),
                                "% decrease."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=sales.chart, 
                   location = ph_location_label(ph_label = "Picture Placeholder 5"))
my_pres <- ph_with(x=my_pres, 
                   value=inventory.chart, 
                   location = ph_location_label(ph_label = "Picture Placeholder 7"))

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
                                               w.sub.title="Source: Office of Financial Management Small Area Estimates Program",
                                               x.type= "Continuous",
                                               est.type = "percent",
                                               w.lwidth = 3,
                                               w.breaks = c("2013","2014","2015","2016","2017","2018","2019","2020"),
                                               w.color = "PrOr") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24)) +
  scale_y_continuous(labels = scales::label_percent(), limits=c(0,1))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Over 60% of housing unit growth is near High Capacity Transit", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2018 and 2020, approximately ", 
                                prettyNum(round(hct.hu.growth.share.since.2018*100,0), big.mark = ","),
                                "% of housing unit growth has occurred within a half mile of current or planned ",
                                "high capacity transit."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=housing.growth.near.hct.chart, 
                   location = ph_location_type(type = "pic"))

# Airport Slide Transition------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Transition Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/seatac-airport-2.jpg")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value="Air Transportation", 
                   location = ph_location_type(type = "title"))

# SeaTac Airport Passengers-----------------------------------------------------------
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

latest.seatac.week <- seatac.airport %>% filter(year=="2022") %>% select(Week) %>% max()
latest.seatac.passengers <- seatac.airport %>% filter(year=="2022" & Week==latest.seatac.week) %>% select(passenger_screenings) %>% pull()
seatac.passengers.2019 <- seatac.airport %>% filter(year=="2019" & Week==latest.seatac.week) %>% select(passenger_screenings) %>% pull()

latest.seatac.passengers.ratio <- latest.seatac.passengers/seatac.passengers.2019

seatac.chart <- create.line.chart(t=seatac.airport, w.x='day', w.y='passenger_screenings', w.g='year', est.type="number", 
                                  w.title="Weekly Passsenger Screenings: 2019 to 2022",
                                  w.sub.title="Source: Port of Seattle SEA Airport Statistics",
                                  d.form = "%B", w.lwidth=3)+
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Passenger Screenings are nearing Pre-Pandemic levels", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("For the last week of July 2022, more than ", 
                                prettyNum(round(latest.seatac.passengers,-2), big.mark = ","),
                                " passengers were screened at Seattle-Tacoma International Airport. This is ",
                                prettyNum(round(latest.seatac.passengers.ratio*100,0), big.mark = ","),
                                "% of the same week in 2019."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=seatac.chart, 
                   location = ph_location_type(type = "pic"))

# SeaTac Airport Cargo -----------------------------------------------------
airport.operations <- process_sea_operations_data(c.yr=2022, c.mo=6)

# Air Cargo
tbl <- airport.operations %>% filter(variable == "CARGO GRAND TOTAL")

latest.seatac.month <- tbl %>% filter(year=="2022") %>% select(month) %>% mutate(month=as.integer(month)) %>% max()
latest.seatac.cargo <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2022" & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.cargo.2019 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2019"  & month==latest.seatac.month) %>% select(estimate) %>% pull()
seatac.cargo.2021 <- tbl %>% mutate(month=as.integer(month)) %>% filter(year=="2021"  & month==latest.seatac.month) %>% select(estimate) %>% pull()

latest.seatac.cargo.ratio <- latest.seatac.cargo/seatac.cargo.2019
latest.seatac.cargo.ratio.2021 <- latest.seatac.cargo/seatac.cargo.2021

seatac.aircargo.chart <- create.line.chart(t=tbl, w.x='equiv_day', w.y='estimate', w.g='year', est.type="number", 
                                           w.title="Monthly Air Cargo: 2019 to 2022", 
                                           w.sub.title="Source: Port of Seattle SEA Airport Statistics",
                                           d.form = "%B", w.lwidth=3,
                                           w.color="psrc_dark") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Air Cargo has slowed in the middle part of 2022", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("For the month of June 2022, more than ", 
                                prettyNum(round(latest.seatac.cargo,-2), big.mark = ","),
                                " metric tons of cargo moved through Seattle-Tacoma International Airport. This is ",
                                prettyNum(round(latest.seatac.cargo.ratio*100,0), big.mark = ","),
                                "% of the same month in 2019 but is approximately ",
                                prettyNum(round(latest.seatac.cargo.ratio.2021*100,0), big.mark = ","),
                                "% of the amount of cargo moved in 2021."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=seatac.aircargo.chart, 
                   location = ph_location_type(type = "pic"))


# Transit Slide Transition------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Transition Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/burien-transit-center.jpg")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value="Transit", 
                   location = ph_location_type(type = "title"))

# Non-SOV Mode Modes ------------------------------------------------------
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

rtp.data <- tibble(name=c("Region"), year = c("2050 RTP"), share = c(rtp.nonsov.shr))

tbl <- bind_rows(tbl, rtp.data)
non.sov.ms.2010 <- tbl %>% filter(year=="2010") %>% select(share) %>% pull()
non.sov.ms.2020 <- tbl %>% filter(year=="2020") %>% select(share) %>% pull()
non.sov.ms.2050 <- tbl %>% filter(year=="2050 RTP") %>% select(share) %>% pull()
transit.ms.2020 <- work.mode %>% filter(name=="Region" & year=="2020" & variable %in% trn.work.trips) %>% select(share) %>% pull()
wfh.ms.2010 <- work.mode %>% filter(name=="Region" & year=="2010" & variable %in% wfh.work.trips) %>% select(share) %>% pull()
wfh.ms.2020 <- work.mode %>% filter(name=="Region" & year=="2020" & variable %in% wfh.work.trips) %>% select(share) %>% pull()

non.sov.ms.chart <- create_column_chart(t=tbl, w.x="year", w.y="share", f="name", 
                                     w.color = "PrOr",
                                     w.title="Non-SOV Mode to Work: 2010, 2015 and 2020",
                                     w.sub.title="Source: ACS 5-yr Data Table B08006") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24)) +
  geom_hline(yintercept = fed.perf.target.nonsov.shr, color='#EC9B21', linetype='dashed', size=2,  show.legend = FALSE)


my_pres <- add_slide(my_pres, layout = "Title with Bullets, Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Fewer people are driving alone to work", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In 2020, ",
                                prettyNum(round(non.sov.ms.2020*100,0), big.mark = ","),
                                "% of all work trips were in a Non-SOV mode, up from ",
                                prettyNum(round(non.sov.ms.2010*100,0), big.mark = ","),
                                "% in 2010. The 2022-2050 RTP forecasts approximately ",
                                prettyNum(round(non.sov.ms.2050*100,0), big.mark = ","),
                                "% of all work trips will be by Non-SOV modes by 2050."),
                   location = ph_location_label(ph_label = "Text Placeholder 4"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("In 2020, the region met the Federal Performance Target for Non-SOV mode share of at least ", 
                                prettyNum(round(fed.perf.target.nonsov.shr*100,0), big.mark = ","),
                                "%"),
                   location = ph_location_label(ph_label = "Text Placeholder 5"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(transit.ms.2020*100,0), big.mark = ","),
                                "% of work trips used transit in 2020"),
                   location = ph_location_label(ph_label = "Text Placeholder 8"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Approximately ", 
                                prettyNum(round(wfh.ms.2020*100,0), big.mark = ","),
                                "% of all workers worked from Home in 2020, up from ",
                                prettyNum(round(wfh.ms.2010*100,0), big.mark = ","),
                                "% in 2010."), 
                   location = ph_location_label(ph_label = "Text Placeholder 10"))
my_pres <- ph_with(x=my_pres, 
                   value=non.sov.ms.chart, 
                   location = ph_location_type(type = "pic"))

# Transit Boardings compared to RTP-------------------------------------------------------
transit.annual <- process_ntd_year_to_date_data(c.yr=full.yr)
transit.uza <- process_ntd_uza_data(yr=full.yr, census.yr="2020")

observed.boardings <- transit.annual %>% 
  filter(variable == "Total Transit" & geography=="Region" & concept=="Transit Boardings") %>% 
  mutate(variable = "Observed") %>% 
  filter(year>="2010") %>%
  select(year, estimate, variable)

observed.forecast.boardings <- observed.boardings %>% 
  filter(year>=2010 & year<=2018) %>% 
  mutate(variable="Forecast") %>%
  select(year, estimate, variable)

forecast.boardings <- seatac.airport <- fread(here('data','rtp-transit-boardings.csv')) %>%
  mutate(year=as.character(year)) %>% 
  select(year, estimate, variable)

boardings.growth.rtp <- bind_rows(list(observed.boardings, observed.forecast.boardings, forecast.boardings)) %>% filter(year<=2025) %>% mutate(year=as.character(year))

observed.boardings.2021 <- boardings.growth.rtp %>% filter(variable=="Observed" & year=="2021") %>% select(estimate) %>% pull()
forecast.boardings.2021 <- boardings.growth.rtp %>% filter(variable=="Forecast" & year=="2021") %>% select(estimate) %>% pull()
observed.boardings.2021.ratio <- observed.boardings.2021/forecast.boardings.2021

boardings.growth.rtp.chart <- create.line.chart(t=boardings.growth.rtp, w.x='year', w.y='estimate', w.g='variable', est.type="number", 
                                             w.title="Annual Transit Boardings: 2010 to 2025",
                                             w.sub.title="Source: National Transit Database & PSRC 2022-2050 RTP",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025"),
                                             w.color = "DkPrLtPr")+
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="The pandemic has had a major impact on transit usage", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Between 2018 and 2022, transit boardings were reduced significantly as a result of the global pandemic. In 2021, ", 
                                prettyNum(round(observed.boardings.2021,-3), big.mark = ","),
                                " boardings happened on the regional transit system, approximately ",
                                prettyNum(round(observed.boardings.2021.ratio*100,0), big.mark = ","), 
                                "% of the forecasted boardings from the RTP in the same year."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=boardings.growth.rtp.chart, 
                   location = ph_location_type(type = "pic"))


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
                                           w.sub.title="Source: National Transit Database",
                                           d.form = "%B", w.lwidth=3,
                                           w.color="psrc_light") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Transit usage is moving up", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Transit boardings in the first half of 2022 were up ", 
                                prettyNum(round((ytd.boardings.2022.to.2021-1)*100,0), big.mark = ","),
                                "% compared to the first half of 2021. Despite the strong ridership growth, overall ridership is still down ",
                                prettyNum(round((ytd.boardings.2022.to.2019-1)*100,0), big.mark = ","), 
                                "% compared to the first six months of 2019."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=monthly.transit.chart, 
                   location = ph_location_type(type = "pic"))

# Boardings by Mode -------------------------------------------------------
transit.ytd <- process_ntd_year_to_date_data(c.yr = "2022")

tbl <- transit.ytd %>%
  filter(geography=="Region" & year>=2019 & concept=="Transit Boardings" & variable!="Total Transit") %>% 
  mutate(variable = gsub("Light Rail, Streetcar & Monorail", "Light Rail", variable))

bus.2019 <- tbl %>% filter(variable=="Bus" & year=="2019") %>% select(estimate) %>% pull()
bus.2022 <- tbl %>% filter(variable=="Bus" & year=="2022") %>% select(estimate) %>% pull()
bus.2022.ratio <- (bus.2022/bus.2019)-1

crt.2019 <- tbl %>% filter(variable=="Commuter Rail" & year=="2019") %>% select(estimate) %>% pull()
crt.2022 <- tbl %>% filter(variable=="Commuter Rail" & year=="2022") %>% select(estimate) %>% pull()
crt.2022.ratio <- (crt.2022/crt.2019)-1
  
van.2019 <- tbl %>% filter(variable=="Vanpool" & year=="2019") %>% select(estimate) %>% pull()
van.2022 <- tbl %>% filter(variable=="Vanpool" & year=="2022") %>% select(estimate) %>% pull()
van.2022.ratio <- (van.2022/van.2019)-1

lrt.2019 <- tbl %>% filter(variable=="Light Rail" & year=="2019") %>% select(estimate) %>% pull()
lrt.2022 <- tbl %>% filter(variable=="Light Rail" & year=="2022") %>% select(estimate) %>% pull()
lrt.2022.ratio <- (lrt.2022/lrt.2019)-1

ytd.transit.by.mode.chart <- create_facet_bar_chart(t=tbl, w.x="year", w.y="estimate", f="concept", g="variable",
                                                    est.type = "number", 
                                                    w.title = "Year to Date Transit Boardings by Mode: 2019 to 2022",
                                                    w.sub.title="Source: National Transit Database",
                                                    w.color = "GnPr") +
  theme(plot.title = element_text(size=28), axis.text = element_text(size=24), legend.text = element_text(size=24))

my_pres <- add_slide(my_pres, layout = "Title with Full Chart and Caption") 
my_pres <- ph_with(x=my_pres, 
                   value="Transit modes are recovering at different rates", 
                   location = ph_location_type(type = "title"))
my_pres <- ph_with(x=my_pres, 
                   value=paste0("Commuter rail is one of most impacted transit modes, still down nearly ", 
                                prettyNum(round(crt.2022.ratio*100,0), big.mark = ","),
                                "% compared to the first half of 2019 whereas light rail is approximately ",
                                prettyNum(round(lrt.2022.ratio*100,0), big.mark = ","), 
                                "% lower compared to the first six months of 2019."), 
                   location = ph_location_type(type = "body"))
my_pres <- ph_with(x=my_pres, 
                   value=ytd.transit.by.mode.chart, 
                   location = ph_location_type(type = "pic"))

# Questions Slide Transition------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Transition Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=external_img(file.path("images/rhs-slide-2400x800.jpg")), 
                   location = ph_location_type(type = "pic"))
my_pres <- ph_with(x=my_pres, 
                   value="Questions: chelmann@psrc.org", 
                   location = ph_location_type(type = "title"))

# Save Presentation -------------------------------------------------------
print(my_pres, target = "puget-sound-trends-gmpb-sept-2022.pptx") 







#layout_summary(my_pres)
#layout_properties (x = my_pres, layout = "Title Slide" )
#layout_properties (x = my_pres, layout = "Transition Slide" )
#layout_properties (x = my_pres, layout = "Title with Full Chart" )
#layout_properties (x = my_pres, layout = "Title with Full Text" )
#layout_properties (x = my_pres, layout = "Title with Full Chart and Caption" )
#layout_properties (x = my_pres, layout = "Title with Bullets, Chart and Caption" )
#layout_properties (x = my_pres, layout = "Two Charts with Caption" )

