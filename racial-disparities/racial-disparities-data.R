library(tidyverse)
library(psrccensus)
library(here)
library(data.table)

# Inputs ------------------------------------------------------------------
census.yr <- 2020
acs.span <- 5

hh.vars <- c("NP","VEH","HINCP",
             "TYPEHUGQ","MRGP","RNTP",
             "ACCESSINET","HISPEED",
             "BLD","VALP","TEN",
             "HRACE","BIN_POVRATIO")

person.vars <- c("AGEP","DIS","HICOV",
                 "SCHL",
                 "JWMNP", "JWTRNS", 
                 "ESR", "NAICSP",
                 "PRACE")

educational.attainment.age <- 25
prek.age <- 6

# Get PSRC PUMS Data for Households and Persons ----------------------------
pums.households <- get_psrc_pums(span=acs.span, dyear=census.yr, level="h", vars=hh.vars) %>%
  mutate(POVERTY = as.character(BIN_POVRATIO)) %>%
  mutate(POVERTY = case_when(
    POVERTY %in% c("2.00 and over") ~ "No",
    !(POVERTY %in% c("2.00 and over")) ~ "Yes")) %>%
  mutate(POVERTY = as.factor(POVERTY)) %>%
  mutate(POC = as.character(HRACE)) %>%
  mutate(POC = case_when(
    POC %in% c("White alone") ~ "No",
    !(POC %in% c("White alone")) ~ "Yes")) %>%
  mutate(POC = as.factor(POC)) %>%
  filter(TYPEHUGQ=="Housing unit") %>%
  mutate(RENT=case_when(
    is.na(RNTP) ~ 0,
    !(is.na(RNTP)) ~ RNTP*12)) %>%
  mutate(MORT=case_when(
    is.na(MRGP) ~ 0,
    !(is.na(MRGP)) ~ MRGP*12)) %>%
  mutate(COST= RENT+MORT) %>%
  mutate(VEH = as.character(VEH)) %>%
  mutate(VEH = case_when(
    VEH == "1 vehicle" ~ "1 vehicle",
    VEH %in% c("2 vehicles","3 vehicles","4 vehicles","5 vehicles","6 or more vehicles") ~ "2 or more vehicles",
    VEH == "No vehicles" ~ "No vehicles")) %>%
  mutate(VEH = as.factor(VEH)) %>%
  mutate(BLD = as.character(BLD)) %>%
  mutate(BLD = case_when(
    BLD == "Mobile home or trailer" ~ "Mobile Home",
    BLD %in% c("One-family house detached","One-family house attached") ~ "Single-Family",
    str_detect(BLD,"Apartments") ~ "Apartment",
    str_detect(BLD,"apartments") ~ "Apartment",
    BLD == "Boat, RV, van, etc." ~ "Other")) %>%
  mutate(BLD = as.factor(BLD)) %>%
  mutate(TEN = as.character(TEN)) %>%
  mutate(TEN = case_when(
    TEN %in% c("Owned with mortgage or loan (include home equity loans)","Owned free and clear") ~ "Own",
    TEN %in% c("Rented","Occupied without payment of rent") ~ "Rent")) %>%
  mutate(TEN = as.factor(TEN)) %>%
  mutate(HISPEED = as.character(HISPEED)) %>%
  mutate(HISPEED = case_when(
    HISPEED == "Yes" ~ "Yes",
    HISPEED == "No" ~ "No",
    is.na(HISPEED) & ACCESSINET %in% c("No access to the Internet at this house, apartment, or mobile home", "Yes, without paying a cell phone company or Internet service provider","Yes, by paying a cell phone company or Internet service provider")  ~ "No")) %>%
  mutate(HISPEED = as.factor(HISPEED)) %>%
  rename(Race=HRACE)

pums.persons <- get_psrc_pums(span=acs.span, dyear=census.yr, level="p", vars=person.vars) %>%
  mutate(POC = as.character(PRACE)) %>%
  mutate(POC = case_when(
    POC %in% c("White alone") ~ "No",
    !(POC %in% c("White alone")) ~ "Yes")) %>%
  mutate(POC = as.factor(POC)) %>%
  mutate(ESR = as.character(ESR)) %>%
  mutate(ESR = case_when(
    ESR %in% c("Civilian employed, with a job but not at work", "Civilian employed, at work", "Armed forces, at work", "Armed forces, with a job but not at work") ~ "Employed",
    ESR %in% c("Not in labor force") ~ "Not in labor force",
    ESR == "Unemployed" ~ "Unemployed")) %>%
  mutate(ESR = as.factor(ESR)) %>%
  mutate(JWTRNS = as.character(JWTRNS)) %>%
  mutate(JWTRNS = case_when(
    JWTRNS %in% c("Car, truck, or van", "Motorcycle", "Taxicab") ~ "Auto",
    JWTRNS %in% c("Bus","Long-distance train or commuter train", "Ferryboat", "Subway or elevated rail", "Light rail, streetcar, or trolley") ~ "Transit",
    JWTRNS %in% c("Walked", "Bicycle") ~ "Bike or Walk",
    JWTRNS %in% c("Worked from home") ~ "Worked from Home",
    JWTRNS %in% c("Other method") ~ "Other")) %>%
  mutate(JWTRNS = as.factor(JWTRNS)) %>%
  rename(Race=PRACE)

# Functions ---------------------------------------------------------------
get_counts_table <- function(d, v, g, c) {
  
  w.data <- psrc_pums_count(so=d, stat_var=v, group_vars = g) %>%
    rename(Estimate=count,MoE=count_moe) %>%
    mutate(MoE = as.integer(MoE), Concept=c)
  
  if(length(g)==1) {
    
    totals <- w.data %>% filter(.data[[g]]=="Total") %>% select(Estimate) %>% pull()
    w.data <- w.data %>% mutate(Share = Estimate/totals, Variable = c) 
    
  } else {
    
    totals <- w.data %>% filter(.data[[v]]=="Total") %>% select(Race,Estimate) %>% rename(Total=Estimate)
    w.data <- left_join(w.data,totals,by="Race") %>% mutate(Share = Estimate/Total) %>% select(-Total) %>% rename(Variable=.data[[v]])
    
  }
  
  return(w.data)
}
  
get_mean_table <- function(d, v, g, c, n.decs=2, min.v=-1) {
    
    est_var <- paste0(v,"_mean")
    moe_var <- paste0(v,"_mean_moe")
    
    d <- d %>% filter(.data[[v]] > min.v)
  
    w.data <- psrc_pums_mean(so=d, stat_var=v, group_vars = g) %>%
      rename(Estimate=.data[[est_var]], MoE=.data[[moe_var]]) %>%
      mutate(Estimate=round(Estimate,n.decs), MoE=round(MoE,n.decs), Concept=c, Variable = c)
    
    totals <- w.data %>% filter(.data[[g]]=="Total") %>% select(Estimate) %>% pull()
    w.data <- w.data %>% mutate(Share = Estimate/totals)
    
    return(w.data)
}

get_median_table <- function(d, v, g, c, n.decs=2, min.v=-1) {
  
  est_var <- paste0(v,"_median")
  moe_var <- paste0(v,"_median_moe")
  
  d <- d %>% filter(.data[[v]] > min.v)
  
  w.data <- psrc_pums_median(so=d, stat_var=v, group_vars = g) %>%
    rename(Estimate=.data[[est_var]], MoE=.data[[moe_var]]) %>%
    mutate(Estimate=round(Estimate,n.decs), MoE=round(MoE,n.decs), Concept=c, Variable = c)
  
  totals <- w.data %>% filter(.data[[g]]=="Total") %>% select(Estimate) %>% pull()
  w.data <- w.data %>% mutate(Share = Estimate/totals)
  
  return(w.data)
}
  
# Household Data by Race ------------------------------------------------------
# Counts
households <- get_counts_table(d=pums.households, v="NP", g=c("Race"), c="Households") %>% filter(Race != "Total") %>% select(-share, -share_moe) %>% mutate(Level="Household")
vehicles <- get_counts_table(d=pums.households, v="VEH", g=c("Race","VEH"), c="Vehicles per Household") %>% filter(Variable=="No vehicles") %>% select(-share, -share_moe) %>% mutate(Level="Household")
internet <- get_counts_table(d=pums.households, v="HISPEED", g=c("Race","HISPEED"), c="High Speed Internet Acces per Household") %>% filter(Variable=="No") %>% select(-share, -share_moe) %>% mutate(Level="Household")
ownernship <- get_counts_table(d=pums.households, v="TEN", g=c("Race","TEN"), c="Home Ownership") %>% filter(Variable=="Own") %>% select(-share, -share_moe) %>% mutate(Level="Household")
type <- get_counts_table(d=pums.households, v="BLD", g=c("Race","BLD"), c="Home Type") %>% filter(Variable=="Single-Family") %>% select(-share, -share_moe) %>% mutate(Level="Household")
# Mean
size <- get_mean_table(d=pums.households, v="NP", g=c("Race"), c="Average Household Size", min.v=0) %>% filter(Race != "Total") %>% mutate(Level="Household")    
cost <- get_mean_table(d=pums.households, v="COST", g=c("Race"), c="Average Annual Housing Cost", n.decs=-2, min.v=0) %>% filter(Race != "Total")  %>% mutate(Level="Household") 
# Median
income <- get_median_table(d=pums.households, v="HINCP", g=c("Race"), c="Median Household Income", n.decs=-2, min.v=0) %>% filter(Race != "Total") %>% mutate(Level="Household")   
value <- get_mean_table(d=pums.households, v="VALP", g=c("Race"), c="Median Home Value", n.decs=-2, min.v=0) %>% filter(Race != "Total") %>% mutate(Level="Household")   

# Calculate Share of Income spent on Housing
t1 <- income %>% select(Race,Estimate) %>% rename(Income=Estimate)
t2 <- cost %>% select(Race,Estimate) %>% rename(Cost=Estimate)
hcr <- left_join(t1,t2,by="Race") %>% 
  mutate(Estimate = round(Cost/Income,2)) %>%
  mutate(Concept="Share of Income Spent on Housing", Variable="Share of Income Spent on Housing", MoE=NA, Share=Estimate, DATA_YEAR=census.yr, COUNTY="Region") %>%
  select(-Cost,-Income) %>%
  mutate(Level="Household")
rm(t1,t2)

# Person Data by Race -----------------------------------------------------
population <- get_counts_table(d=pums.persons, v="AGEP", g=c("Race"), c="Population") %>% filter(Race != "Total") %>% select(-share, -share_moe) %>% mutate(Level="Person")
disability <- get_counts_table(d=pums.persons, v="DIS", g=c("Race","DIS"), c="Disability Status") %>% filter(Variable=="With a disability") %>% select(-share, -share_moe) %>% mutate(Level="Person")
insurance <- get_counts_table(d=pums.persons, v="HICOV", g=c("Race","HICOV"), c="Health Insurance Coverage") %>% filter(Variable=="No health insurance coverage") %>% select(-share, -share_moe) %>% mutate(Level="Person")

# For Employment, first filter out people that can't work (in the Census case, 16 and under)
temp <- pums.persons %>% filter(!(is.na(ESR)))
employment <- get_counts_table(d=temp, v="ESR", g=c("Race","ESR"), c="Employment Status") %>% filter(Variable=="Unemployed") %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

# For Educational Attainment, we want to filter out anyone under the age defined in the educational attainment (most commonly under 25) and then combine some groups
temp <- pums.persons %>% 
  filter(AGEP>=educational.attainment.age) %>%
  mutate(SCHL =as.character(SCHL)) %>%
  mutate(SCHL = case_when(
    SCHL %in% c("No schooling completed","Nursery school, preschool","Kindergarten", "12th grade - no diploma") ~ "Did not Graduate High School",
    str_detect(SCHL, "Grade") ~ "Did not Graduate High School",
    SCHL %in% c("Regular high school diploma","GED or alternative credential") ~ "High School Diploma or Equivalent",
    SCHL %in% c("Some college, but less than 1 year","1 or more years of college credit, no degree") ~ "Some College",
    SCHL %in% c("Associate's degree") ~ "Associates Degree",
    SCHL %in% c("Bachelor's degree") ~ "At least a Bachelors Degree",
    SCHL %in% c("Master's degree","Professional degree beyond a bachelor's degree","Doctorate degree") ~ "At least a Bachelors Degree")) %>%
  mutate(SCHL = as.factor(SCHL))
bachelors <- get_counts_table(d=temp, v="SCHL", g=c("Race","SCHL"), c="Educational Attainment") %>% filter(Variable%in%c("At least a Bachelors Degree")) %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

# For Educational Attainment, we want to filter out anyone under the age defined in the educational attainment (most commonly under 25) and then combine some groups
temp <- pums.persons %>% 
  filter(AGEP>=educational.attainment.age) %>%
  mutate(SCHL =as.character(SCHL)) %>%
  mutate(SCHL = case_when(
    SCHL %in% c("No schooling completed","Nursery school, preschool","Kindergarten", "12th grade - no diploma") ~ "Did not Graduate High School",
    str_detect(SCHL, "Grade") ~ "Did not Graduate High School",
    SCHL %in% c("Regular high school diploma","GED or alternative credential") ~ "High School Diploma or Equivalent",
    SCHL %in% c("Some college, but less than 1 year","1 or more years of college credit, no degree") ~ "Some College",
    SCHL %in% c("Associate's degree") ~ "Associates Degree",
    SCHL %in% c("Bachelor's degree") ~ "Bachelors Degree",
    SCHL %in% c("Master's degree","Professional degree beyond a bachelor's degree","Doctorate degree") ~ "Masters, Professional or PhD Degree")) %>%
  mutate(SCHL = as.factor(SCHL))
terminal <- get_counts_table(d=temp, v="SCHL", g=c("Race","SCHL"), c="Educational Attainment") %>% filter(Variable%in%c("Masters, Professional or PhD Degree")) %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

# For Kindergarten Readiness, look at all kids under 6 that are not in Kindergarten yet
temp <- pums.persons %>% filter(AGEP < prek.age & SCHL %in% c("No schooling completed","Nursery school, preschool"))
kindergarten <- get_counts_table(d=temp, v="SCHL", g=c("Race","SCHL"), c="Kindergarten Readiness") %>% filter(Variable=="Nursery school, preschool") %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

# For Travel Time to work, filter out anyone under age 16
temp <- pums.persons %>% filter(!(is.na(JWMNP)))
time <- get_mean_table(d=temp, v="JWMNP", g=c("Race"), c="Average Travel Time to Work", min.v=0)  %>% filter(Race != "Total") %>% mutate(Level="Person")
rm(temp)

# For Travel Mode to work, filter out anyone under age 16
temp <- pums.persons %>% filter(!(is.na(JWTRNS)))
mode <- get_counts_table(d=temp, v="JWTRNS", g=c("Race","JWTRNS"), c="Mode to Work") %>% filter(Variable=="Transit") %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

# For Work from Home, filter out anyone under age 16
temp <- pums.persons %>% filter(!(is.na(JWTRNS)))
wfh <- get_counts_table(d=temp, v="JWTRNS", g=c("Race","JWTRNS"), c="Mode to Work") %>% filter(Variable=="Worked from Home") %>% select(-share, -share_moe) %>% mutate(Level="Person")
rm(temp)

age <- get_mean_table(d=pums.persons, v="AGEP", g=c("Race"), c="Average Age", min.v=-1) %>% filter(Race != "Total") %>% mutate(Level="Person")

# Essential Workers -------------------------------------------------------
essential.lookups <- fread(here('data',"essential-workers.csv"))
essential.codes <- essential.lookups %>% filter(Essential==1) %>% select(Variable) %>% pull()
non.essential.codes <- essential.lookups %>% filter(Essential==0) %>% select(Variable) %>% pull()
partial.essential.codes <- essential.lookups %>% filter(Essential==2) %>% select(Variable) %>% pull()

persons.naics <- get_psrc_pums(span=acs.span, dyear=census.yr, level="p", vars=c("NAICSP","PRACE"), labels = FALSE) %>%
  mutate(NAICSP = as.character((NAICSP))) %>%
  mutate(Essential = case_when(
    NAICSP %in% essential.codes ~ "Essential Worker",
    NAICSP %in% partial.essential.codes ~ "Somewhat-Essential Worker",
    NAICSP %in% non.essential.codes ~ "Non-Essential Worker")) %>%
  mutate(Essential = as.factor(Essential)) %>%
  filter(!(is.na(Essential))) %>%
  mutate(Race = case_when(
    PRACE == "I" ~ "American Indian or Alaskan Native",
    PRACE == "H" ~ "Hispanic or Latino",
    PRACE == "1" ~ "White",
    PRACE == "2" ~ "Black or African American",
    PRACE == "6" ~ "Asian",
    PRACE == "7" ~ "Native Hawaiian and Other Pacific Islander",
    PRACE == "8" ~ "Some Other Race",
    PRACE == "9" ~ "Two or More Races"))

essential <- get_counts_table(d=persons.naics, v="Essential", g=c("Race","Essential"), c="Essential Workers") %>% filter(Variable=="Essential Worker") %>% select(-share, -share_moe) %>% mutate(Level="Person")
                  
# Life Expectancy ---------------------------------------------------------
life.expectancy <- read_csv(here('data','life-expectancy-by-race.csv')) %>%
  rename(Estimate=`2019`) %>%
  mutate(Concept="Life Expectancy", MoE=NA, Share=NA, Variable="Rate", DATA_YEAR=census.yr, COUNTY="State") %>%
  select(Race, Estimate, MoE, Concept, Variable, Share, DATA_YEAR, COUNTY) %>% mutate(Level="Person")

# Infant Mortality --------------------------------------------------------
births <- read_csv(here('data','births-by-race.csv')) %>%
  pivot_longer(!Race) %>%
  filter(name %in% c("2015","2016","2017","2018","2019")) %>%
  group_by(Race) %>%
  summarise(Estimate = sum(value)) %>%
  mutate(Concept="Births", MoE=NA, Share=NA, Variable="Total Births 2015 to 2019", DATA_YEAR=census.yr, COUNTY="State") %>%
  select(Race, Estimate, MoE, Concept, Variable, Share, DATA_YEAR, COUNTY)

infant.deaths <- read_csv(here('data','infant-deaths-by-race.csv')) %>%
  pivot_longer(!Race) %>%
  filter(name %in% c("2015","2016","2017","2018","2019")) %>%
  group_by(Race) %>%
  summarise(Estimate = sum(value)) %>%
  mutate(Concept="Infant Deaths", MoE=NA, Share=NA, Variable="Total Deaths under 1yr 2015 to 2019", DATA_YEAR=census.yr, COUNTY="State") %>%
  select(Race, Estimate, MoE, Concept, Variable, Share, DATA_YEAR, COUNTY)

t1 <- births %>% select(Race, Estimate) %>% rename(Births=Estimate)
t2 <- infant.deaths %>% select(Race, Estimate) %>% rename(Deaths=Estimate)
infant.mortality <- left_join(t1,t2,by=c("Race")) %>%
  mutate(Estimate=round((Deaths/(Births/1000)),2)) %>%
  mutate(Concept="Infant Mortality", MoE=NA, Share=NA, Variable="Infant Mortality Rate per 1000 Births 2015 to 2019", DATA_YEAR=census.yr, COUNTY="State") %>%
  select(Race, Estimate, MoE, Concept, Variable, Share, DATA_YEAR, COUNTY) %>% mutate(Level="Person")

rm(births, infant.deaths, t1, t2)

# Output Final Data -------------------------------------------------------
racial.disparities.data <- bind_rows(list(age, bachelors, cost, disability, employment, essential, hcr, households, income, 
                                          infant.mortality, insurance, internet, kindergarten, life.expectancy, mode,
                                          ownernship, population, size, terminal, time, type, value, vehicles, wfh)) %>%
  mutate(Race = gsub(" Alone", "", Race)) %>%
  mutate(Race = gsub(" alone", "", Race)) %>%
  mutate(Race = gsub("White", "Non-Hispanic White", Race)) %>%
  mutate(Race = gsub("Hispanic or Latino", "Hispanic or LatinX", Race)) %>%
  rename(Geography=COUNTY, Year=DATA_YEAR)

fwrite(racial.disparities.data, here('data',paste0("pums_equity_data_",census.yr,"_5yr.csv")))




