library(psrccensus)
library(psrctrends)
library(psrcplot)
library(psrcslides)

library(tidyverse)
library(openxlsx)
library(lubridate)
library(tidycensus)
library(sf)
library(officer)
library(data.table)

install_psrc_fonts()
psrc_pres = read_pptx(system.file('extdata', 'psrc-trends-template.pptx', package='psrcslides'))
                             
# Cover Slide -------------------------------------------------------------
psrc_pres <- add_cover_slide(p = psrc_pres,
                             p.title = "PSRC Trends",
                             p.mtg = "Passport to 2044",
                             p.date = "September 2022")

# Population Growth -------------------------------
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

pop.growth.vision <- bind_rows(list(observed.pop, observed.forecast.pop, forecast.pop)) %>% mutate(Year=as.character(Year))

pop.growth.vision.chart <- create.line.chart(t=pop.growth.vision, w.x='Year', w.y='Estimate', w.g='Variable', est.type="number", 
                                             w.title="Annual Population Forecast: 2010 to 2050",
                                             x.type= "Continuous",
                                             w.lwidth = 3,
                                             w.breaks = c("2010","2015","2020","2025","2030", "2035", "2040", "2045", "2050"),
                                             w.color = "DkPrLtPr") +
  labs(caption = paste0("Source: Office of Financial Management Intercensal Population Estimates & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                          p.title = "We are planning for 5.8 Million People by 2050",
                                          p.chart = pop.growth.vision.chart)

# Components of Population Change -----------------------------------------
ofm.pop.chg <- get_ofm_postcensal_pop_change()
tbl <- ofm.pop.chg %>% filter(Jurisdiction=="Region" & Year >= "2010")

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

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                          p.title = "Migration accounted for 67% of the growth in the past 10 years",
                                          p.chart = population.type.change.chart)

# Regional Housing Production ---------------------------------------------
ofm.housing <- get_ofm_postcensal_housing()

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

housing.growth.vision.chart <- create.line.chart(t=vision.housing, w.x='Year', w.y='Estimate', w.g='Variable', est.type="number", 
                                                 w.title="Total Housing Unit Forecast: 2010 to 2050",
                                                 x.type= "Continuous",
                                                 w.lwidth = 3,
                                                 w.breaks = c("2010","2015","2020","2025","2030", "2035", "2040", "2045", "2050"),
                                                 w.color = "DkGnLtGn")+
  labs(caption = paste0("Source: Office of Financial Management & PSRC Regional Macroeconomic Forecast")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24)) +
  scale_y_continuous(labels = scales::label_comma(), limits=c(1000000,3000000))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "We will need almost 1 million new housing units by 2050",
                                  p.chart = housing.growth.vision.chart)

# Housing Growth by Housing Type ------------------------------------------------------------
sf <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Single-Family") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2011) %>%
  mutate(Year = as.character(Year))

mf <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Multi-Family") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2011) %>%
  mutate(Year = as.character(Year))

mh <- ofm.housing %>% 
  filter(Jurisdiction == "Region" & Variable == "Mobile Home") %>%
  mutate(Delta = round(Estimate - lag(Estimate),-1)) %>%
  filter(Year >=2011) %>%
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
                                                 w.color = "psrc_light") +
  labs(caption = paste0("Source: Office of Financial Management")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "68% of new housing units were multi-family in the past 10 years",
                                  p.chart = housing.type.change.chart)

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

work.mode <- get_acs_recs(geography = 'county',table.names = c('B08006'), years=c(2010,2015,2020), acs.type = 'acs5') %>% filter(variable %in% all.trips)
mode.totals <- work.mode %>% filter(variable %in% total.work.trips) %>% select(name, year, estimate) %>% rename(total=estimate)
work.mode <- left_join(work.mode, mode.totals, by=c("name","year")) %>% mutate(share=estimate/total)

tbl <- work.mode %>%
  filter(variable %in% wfh.work.trips) %>%
  filter(name=="Region") %>%
  select(name, year, share) %>%
  mutate(year=as.character(year))

wfh.chart <- create_column_chart(t=tbl, w.x="year", w.y="share", f="name", 
                                 w.color = "psrc_light",
                                 w.title="Work from Home: 2010, 2015 and 2020") +
  labs(caption = paste0("Source: ACS 5yr Data Table B08006")) +
  theme(plot.caption = element_text(family="Poppins", size=22, color="#4C4C4C", lineheight = 0.5),
        plot.title = element_text(size=28), 
        axis.text = element_text(size=24), 
        legend.text = element_text(size=24))

psrc_pres <- add_full_chart_slide(p = psrc_pres, 
                                  p.title = "Working from home is still changing",
                                  p.chart = wfh.chart)

# Question Slide ------------------------------------------------------
psrc_pres <- add_transition_slide(p=psrc_pres,
                                  p.title="Questions: chelmann@psrc.org",
                                  p.img=('X:/DSA/shiny-uploads/images/rhs-slide-2400x800.jpg'))

# Write Out Slides to Disc ------------------------------------------------
print(psrc_pres, target = "puget-sound-trends-sept-2022.pptx") 



