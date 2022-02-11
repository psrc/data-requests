# Packages ----------------------------------------------------------------

# Data Wrangling
library(tidyverse)
library(data.table)

# Packages for Maps
library(sf)
library(leaflet)
library(mapview)

# Packages for Tables
library(scales)
library(kableExtra)

# Packages for Database Connections
library(odbc)
library(DBI)

# Functions ---------------------------------------------------------------
create.static.map <- function(poly.lyr, route.lyr, poly.clr, route.clr, img.pth, poly.title, route.title) {
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>%
    
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolylines(data = route.lyr,
                 color = route.clr,
                 weight = 3,
                 fillColor = route.clr) %>%
    
    addPolygons(data=poly.lyr,
                fillOpacity = 0.25,
                fillColor = poly.clr,
                opacity = 0.25,
                weight = 0.5,
                color = "#BCBEC0",
                options = leafletOptions(pane = "polygons"),
                dashArray = "") %>%
    
    addLegend(colors=c(poly.clr),
              labels=c(poly.title),
              position = "bottomright",
              title = "") %>%
    
    addLegend(colors=c(route.clr),
              labels=c(route.title),
              position = "bottomright",
              title = "")
  
  mapshot(m, file = img.pth)
  
}

# General Inputs ----------------------------------------------------------
census.year <- 2019
pm.network.base <- st_read(file.path(getwd(),"/data/base/PM_edges.shp"))
pm.network.plan <- st_read(file.path(getwd(),"/data/plan/PM_edges.shp"))

wgs84 <- 4326
spn <- 2285 

model.runs <- c("base", "plan")

corridor.sections <- list(list("sr7_so_sr512.txt","SR7-1"), list("sr7_sr512_to_38th.txt","SR7-2"), list("pac_Ave_38th_to_11th.txt","SR7-3"),
                          list("yakima_so_sr512.txt","Yakima-1"), list("yakima_sr512_to_38th.txt","Yakima-2"), list("yakima_38th_to_11th.txt","Yakima-3"),
                          list("portland_sr512_to_38th.txt","Portland-2"), list("portland_38th_to_11th.txt","Portland-3"),
                          list("canyon_so_sr512.txt","Canyon-1"), list("canyon_sr512_to_38th.txt","Canyon-2"),
                          list("i5_so_sr512.txt","I5-1"), list("i5_no_sr512.txt","I5-2"),
                          list("ew_so_512.txt","EW-1"), list("ew_no_512.txt","EW-2"))

transit.routes.lyr <- st_read(file.path(getwd(),"/data/plan/directions_2050.shp")) %>% 
  st_transform(wgs84) %>%
  filter(line_id=="3b7a72ab") %>%
  select(line_id,line_name)

transit.routes.buffer <- transit.routes.lyr %>%
  st_transform(spn) %>%
  st_buffer(dist=(0.5*5280))


# TAZ Geography -----------------------------------------------------------
taz.lyr <- st_read(file.path(getwd(),"/data/taz2010nowater.shp")) %>% 
  st_transform(spn) %>%
  select(TAZ) 

pac.ave.taz <- st_intersection(taz.lyr, transit.routes.buffer) %>%
  st_drop_geometry() %>%
  select(TAZ) %>%
  unique() %>%
  pull()

pac.ave.taz.lyr <- taz.lyr %>% filter(TAZ %in% pac.ave.taz) %>% st_transform(wgs84)

taz.data <- as_tibble(fread(file.path(getwd(),paste0("/output/demographic_model_data.csv")))) %>%
  select(taz_id, `Total-Jobs`, `Total-Households`, `Total-Population`, model_run) %>%
  filter(taz_id %in% pac.ave.taz) %>%
  group_by(taz_id, model_run) %>%
  summarize(across(everything(),sum))

create.static.map(poly.lyr=pac.ave.taz.lyr, poly.clr="#91268F", poly.title="Traffic Analysis Zones",
                  img.pth=file.path(getwd(),"/images/corridor_taz.png"),
                  route.lyr=transit.routes.lyr, route.clr="#F05A28", route.title="Pacific Avenue BRT")

# Tract Equity Geography --------------------------------------------------------
tract.lyr <- st_read(file.path(getwd(),"/data/tract2010_nowater.shp")) %>% 
  st_transform(spn) %>%
  select(GEOID10) %>%
  rename(geoid=GEOID10)

server_name <- "AWS-PROD-SQL\\SOCKEYE"
database_name <- "Elmer"

db_con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server_name,
                    database = database_name,
                    trusted_connection = "yes"
)

# Queries to pass to Elmer functions
poverty_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.poverty_equity_geographies(",census.year,",'Tract')")
people_of_color_query <- paste0("SELECT geoid, equity_geog_vs_reg_total FROM Census.racial_equity_geographies(",census.year,",'Tract')")

# Tract Data by Geography
tract.poverty <- as_tibble(DBI::dbGetQuery(db_con, poverty_query)) %>% rename(poverty=equity_geog_vs_reg_total)
tract.people.of.color <- as_tibble(DBI::dbGetQuery(db_con, people_of_color_query)) %>%  rename(people_of_color=equity_geog_vs_reg_total)

dbDisconnect(db_con)

tracts <- tract.lyr %>% st_drop_geometry()

tracts <- list(tracts, tract.poverty, tract.people.of.color) %>%
  reduce(left_join, by = "geoid")

tract.lyr <- left_join(tract.lyr, tracts, by = "geoid")

pac.ave.tracts <- st_intersection(tract.lyr, transit.routes.buffer) %>%
  st_drop_geometry() %>%
  select(geoid) %>%
  unique() %>%
  pull()

pac.ave.tract.lyr <- tract.lyr %>% filter(geoid %in% pac.ave.tracts) %>% st_transform(wgs84)
pac.ave.tract.poc.lyr <- tract.lyr %>% filter(geoid %in% pac.ave.tracts & people_of_color==1) %>% st_transform(wgs84)
pac.ave.tract.pov.lyr <- tract.lyr %>% filter(geoid %in% pac.ave.tracts & poverty==1) %>% st_transform(wgs84)

create.static.map(poly.lyr=pac.ave.tract.poc.lyr, poly.clr="#91268F", poly.title="People of Color over Regional Average",
                  img.pth=file.path(getwd(),"/images/corridor_tract_poc.png"),
                  route.lyr=transit.routes.lyr, route.clr="#F05A28", route.title="Pacific Avenue BRT")

create.static.map(poly.lyr=pac.ave.tract.pov.lyr, poly.clr="#8CC63E", poly.title="People of Lower Income over Regional Average",
                  img.pth=file.path(getwd(),"/images/corridor_tract_pov.png"),
                  route.lyr=transit.routes.lyr, route.clr="#F05A28", route.title="Pacific Avenue BRT")

num.pac.ave.tracts <- pac.ave.tract.lyr %>% st_drop_geometry() %>% select(geoid) %>% pull() %>% length()
num.pac.ave.tracts.poc <- pac.ave.tract.poc.lyr %>% st_drop_geometry() %>% select(geoid) %>% pull() %>% length()
num.pac.ave.tracts.pov <- pac.ave.tract.pov.lyr %>% st_drop_geometry() %>% select(geoid) %>% pull() %>% length()

# Corridor IDs -------------------------------------------------------
corridor.flags <- NULL 
for (run in model.runs) {
  for (sections in corridor.sections) {
    s <- NULL
    s <- as_tibble(fread(file.path(getwd(),paste0("/data/",run,"/",sections[[1]])))) %>%
      select(id) %>%
      rename(ij=id) %>%
      mutate(model_run=run) %>%
      mutate(corridor=sections[[2]])
    ifelse(is.null(corridor.flags), corridor.flags <- s, corridor.flags <- bind_rows(corridor.flags, s))
  }
}

rm(s)

# Corridor Maps -----------------------------------------------------------
corridor.links <- corridor.flags %>% select(ij) %>% distinct() %>% pull() 

corridor.lyr <- pm.network.plan %>%
  select(id) %>%
  rename(ij=id) %>%
  filter(ij %in% corridor.links)

corridor.lyr <- left_join(corridor.lyr, corridor.flags, by=c("ij")) %>%
  filter(model_run=="plan")

sr7.lyr <- corridor.lyr %>% filter(str_detect(corridor,"SR7")) %>% st_transform(wgs84)
no.canyon.lyr <- corridor.lyr %>% filter(str_detect(corridor,"SR7") | str_detect(corridor,"Portland") | str_detect(corridor,"Yakima")) %>% st_transform(wgs84)
canyon.lyr <- corridor.lyr %>% filter(str_detect(corridor,"SR7") | str_detect(corridor,"Portland") | str_detect(corridor,"Yakima") | str_detect(corridor,"Canyon")) %>% st_transform(wgs84)
i5.lyr <- corridor.lyr %>% filter(str_detect(corridor,"I5")) %>% st_transform(wgs84)

create.static.map(poly.lyr=pac.ave.taz.lyr, poly.clr="#91268F", poly.title="Traffic Analysis Zones",
                  img.pth=file.path(getwd(),"/images/pac_avenue_screenline.png"),
                  route.lyr=sr7.lyr, route.clr="#F05A28", route.title="Pacific Avenue")

create.static.map(poly.lyr=pac.ave.taz.lyr, poly.clr="#91268F", poly.title="Traffic Analysis Zones",
                  img.pth=file.path(getwd(),"/images/no_canyon_screenline.png"),
                  route.lyr=no.canyon.lyr, route.clr="#F05A28", route.title="Screenline without Canyon Road")

create.static.map(poly.lyr=pac.ave.taz.lyr, poly.clr="#91268F", poly.title="Traffic Analysis Zones",
                  img.pth=file.path(getwd(),"/images/canyon_screenline.png"),
                  route.lyr=canyon.lyr, route.clr="#F05A28", route.title="Screenline with Canyon Road")


# Link Level Outputs ----------------------------------------------------------
link.outputs <- NULL
for (run in model.runs) {
  n <- NULL
  n <- as_tibble(fread(file.path(getwd(),paste0("/data/",run,"/network_results.csv")))) %>%
    filter(volume_delay_func<9) %>%
    select(ij, length, num_lanes, `@tveh`, tod) %>%
    rename(total_volume=`@tveh`) %>%
    mutate(total_volume = round(total_volume,-1), length = round(length,2)) %>%
    mutate(model_run=run)
  
  ifelse(is.null(link.outputs), link.outputs <- n, link.outputs <- bind_rows(link.outputs, n))
  
}

rm(n)

link.outputs <- left_join(link.outputs, corridor.flags, by=c("ij","model_run")) %>%
  mutate(vmt=total_volume*length)

yrs <- 2030 - 2018

# All Corridors
all.corridors.base <- link.outputs %>% filter(!(is.na(corridor))) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
all.corridors.future <- link.outputs %>% filter(!(is.na(corridor))) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
all.corridors.growth <- (all.corridors.future - all.corridors.base) / all.corridors.base

all.corridors.lagr <- ((all.corridors.future - all.corridors.base) / (all.corridors.base*yrs))
all.corridors.cagr <- ((all.corridors.future/all.corridors.base) ^(1/yrs))-1

# SR 7
sr7.corridors.base <- link.outputs %>% filter(corridor %in% c("SR7-1","SR7-2","SR7-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
sr7.corridors.future <- link.outputs %>% filter(corridor %in% c("SR7-1","SR7-2","SR7-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
sr7.corridors.growth <- (sr7.corridors.future - sr7.corridors.base) / sr7.corridors.base

sr7.corridors.lagr <- ((sr7.corridors.future - sr7.corridors.base) / (sr7.corridors.base*yrs))
sr7.corridors.cagr <- ((sr7.corridors.future/sr7.corridors.base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
sr7.south.base <- link.outputs %>% filter(corridor %in% c("SR7-1")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
sr7.south.future <- link.outputs %>% filter(corridor %in% c("SR7-1")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
sr7.south.growth <- (sr7.south.future - sr7.south.base) / sr7.south.base

sr7.south.lagr <- ((sr7.south.future - sr7.south.base) / (sr7.south.base*yrs))
sr7.south.cagr <- ((sr7.south.future/sr7.south.base) ^(1/yrs))-1

# SR 512 to 38th (to use for north segment)
sr7.north.base <- link.outputs %>% filter(corridor %in% c("SR7-2")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
sr7.north.future <- link.outputs %>% filter(corridor %in% c("SR7-2")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
sr7.north.growth <- (sr7.north.future - sr7.north.base) / sr7.north.base

sr7.north.lagr <- ((sr7.north.future - sr7.north.base) / (sr7.north.base*yrs))
sr7.north.cagr <- ((sr7.north.future/sr7.north.base) ^(1/yrs))-1

# Yakima Avenue
yak.corridors.base <- link.outputs %>% filter(corridor %in% c("Yakima-1","Yakima-2","Yakima-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
yak.corridors.future <- link.outputs %>% filter(corridor %in% c("Yakima-1","Yakima-2","Yakima-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
yak.corridors.growth <- (yak.corridors.future - yak.corridors.base) / yak.corridors.base

yak.corridors.lagr <- ((yak.corridors.future - yak.corridors.base) / (yak.corridors.base*yrs))
yak.corridors.cagr <- ((yak.corridors.future/yak.corridors.base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
yak.south.base <- link.outputs %>% filter(corridor %in% c("Yakima-1")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
yak.south.future <- link.outputs %>% filter(corridor %in% c("Yakima-1")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
yak.south.growth <- (yak.south.future - yak.south.base) / yak.south.base

yak.south.lagr <- ((yak.south.future - yak.south.base) / (yak.south.base*yrs))
yak.south.cagr <- ((yak.south.future/yak.south.base) ^(1/yrs))-1

# North of SR 512 (to use for 112th)
yak.north.base <- link.outputs %>% filter(corridor %in% c("Yakima-2")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
yak.north.future <- link.outputs %>% filter(corridor %in% c("Yakima-2")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
yak.north.growth <- (yak.north.future - yak.north.base) / yak.north.base

yak.north.lagr <- ((yak.north.future - yak.north.base) / (yak.north.base*yrs))
yak.north.cagr <- ((yak.north.future/yak.north.base) ^(1/yrs))-1

# Portland Avenue
port.corridors.base <- link.outputs %>% filter(corridor %in% c("Portland-1","Portland-2","Portland-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
port.corridors.future <- link.outputs %>% filter(corridor %in% c("Portland-1","Portland-2","Portland-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
port.corridors.growth <- (port.corridors.future - port.corridors.base) / port.corridors.base

port.corridors.lagr <- ((port.corridors.future - port.corridors.base) / (port.corridors.base*yrs))
port.corridors.cagr <- ((port.corridors.future/port.corridors.base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
port.south.base <- link.outputs %>% filter(corridor %in% c("Portland-2")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
port.south.future <- link.outputs %>% filter(corridor %in% c("Portland-2")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
port.south.growth <- (port.south.future - port.south.base) / port.south.base

port.south.lagr <- ((port.south.future - port.south.base) / (port.south.base*yrs))
port.south.cagr <- ((port.south.future/port.south.base) ^(1/yrs))-1

# North of SR 512 (to use for 112th)
port.north.base <- link.outputs %>% filter(corridor %in% c("Portland-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
port.north.future <- link.outputs %>% filter(corridor %in% c("Portland-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
port.north.growth <- (port.north.future - port.north.base) / port.north.base

port.north.lagr <- ((port.north.future - port.north.base) / (port.north.base*yrs))
port.north.cagr <- ((port.north.future/port.north.base) ^(1/yrs))-1

# Canyon Road
cany.corridors.base <- link.outputs %>% filter(corridor %in% c("Canyon-1","Canyon-2","Canyon-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
cany.corridors.future <- link.outputs %>% filter(corridor %in% c("Canyon-1","Canyon-2","Canyon-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
cany.corridors.growth <- (cany.corridors.future - cany.corridors.base) / cany.corridors.base

cany.corridors.lagr <- ((cany.corridors.future - cany.corridors.base) / (cany.corridors.base*yrs))
cany.corridors.cagr <- ((cany.corridors.future/cany.corridors.base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
cany.south.base <- link.outputs %>% filter(corridor %in% c("Canyon-1")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
cany.south.future <- link.outputs %>% filter(corridor %in% c("Canyon-1")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
cany.south.growth <- (cany.south.future - cany.south.base) / cany.south.base

cany.south.lagr <- ((cany.south.future - cany.south.base) / (cany.south.base*yrs))
cany.south.cagr <- ((cany.south.future/cany.south.base) ^(1/yrs))-1

# North of SR 512 (to use for 112th)
cany.north.base <- link.outputs %>% filter(corridor %in% c("Canyon-2")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
cany.north.future <- link.outputs %>% filter(corridor %in% c("Canyon-2")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
cany.north.growth <- (cany.north.future - cany.north.base) / cany.north.base

cany.north.lagr <- ((cany.north.future - cany.north.base) / (cany.north.base*yrs))
cany.north.cagr <- ((cany.north.future/cany.north.base) ^(1/yrs))-1

# I-5
i5.corridors.base <- link.outputs %>% filter(corridor %in% c("I5-1","I5-2","I5-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
i5.corridors.future <- link.outputs %>% filter(corridor %in% c("I5-1","I5-2","I5-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
i5.corridors.growth <- (i5.corridors.future - i5.corridors.base) / i5.corridors.base

i5.corridors.lagr <- ((i5.corridors.future - i5.corridors.base) / (i5.corridors.base*20))
i5.corridors.cagr <- ((i5.corridors.future/i5.corridors.base) ^(1/yrs))-1

# SR 7 / Portland / Yakima / Canyon
base <- sr7.corridors.base + yak.corridors.base + port.corridors.base + cany.corridors.base
future <- sr7.corridors.future + yak.corridors.future + port.corridors.future + cany.corridors.future
corridors.growth <- (future - base) / base

corridors.lagr <- ((future - base) / (base*yrs))
corridors.cagr <- ((future/base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
base <- sr7.south.base + yak.south.base + port.south.base + cany.south.base
future <- sr7.south.future + yak.south.future + port.south.future + cany.south.future
corridors.growth <- (future - base) / base

corridors.south.lagr <- ((future - base) / (base*yrs))
corridors.south.cagr <- ((future/base) ^(1/yrs))-1

# North of SR 512 (to use for 112th)
base <- sr7.north.base + yak.north.base + port.north.base + cany.north.base
future <- sr7.north.future + yak.north.future + port.north.future + cany.north.future
corridors.growth <- (future - base) / base

corridors.north.lagr <- ((future - base) / (base*yrs))
corridors.north.cagr <- ((future/base) ^(1/yrs))-1

# SR 7 / Portland / Yakima
base <- sr7.corridors.base + yak.corridors.base + port.corridors.base
future <- sr7.corridors.future + yak.corridors.future + port.corridors.future
corridors.growth <- (future - base) / base

corridors.no.cany.lagr <- ((future - base) / (base*yrs))
corridors.no.cany.cagr <- ((future/base) ^(1/yrs))-1

# South of SR 512 (to use for 112th)
base <- sr7.south.base + yak.south.base + port.south.base
future <- sr7.south.future + yak.south.future + port.south.future
corridors.growth <- (future - base) / base

corridors.south.no.cany.lagr <- ((future - base) / (base*yrs))
corridors.south.no.cany.cagr <- ((future/base) ^(1/yrs))-1

# North of SR 512 (to use for 112th)
base <- sr7.north.base + yak.north.base + port.north.base
future <- sr7.north.future + yak.north.future + port.north.future
corridors.growth <- (future - base) / base

corridors.north.no.cany.lagr <- ((future - base) / (base*yrs))
corridors.north.no.cany.cagr <- ((future/base) ^(1/yrs))-1

# East-West Corridors
# South of SR 512 (to use for 112th)
ew.south.base <- link.outputs %>% filter(corridor %in% c("EW-1")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
ew.south.future <- link.outputs %>% filter(corridor %in% c("EW-1")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
ew.south.growth <- (ew.south.future - ew.south.base) / ew.south.base

ew.south.lagr <- ((ew.south.future - ew.south.base) / (ew.south.base*(2045-2017)))
ew.south.cagr <- ((ew.south.future/ew.south.base) ^(1/(2045-2017)))-1

# SR 512 to 38th (to use for north segment)
ew.north.base <- link.outputs %>% filter(corridor %in% c("EW-2")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
ew.north.future <- link.outputs %>% filter(corridor %in% c("EW-2")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
ew.north.growth <- (ew.north.future - ew.north.base) / ew.north.base

ew.north.lagr <- ((ew.north.future - ew.north.base) / (ew.north.base*(2045-2018)))
ew.north.cagr <- ((ew.north.future/ew.north.base) ^(1/(2045-2018)))-1

# Population and Employment Growth ---------------------------------------
t <- as_tibble(fread(file.path(getwd(),paste0("/data/","/wsdot-pop-jobs-growth.csv")))) %>%
  filter(Year %in% c("2017","2045")) %>%
  pivot_longer(cols=(-Year)) %>%
  mutate(value = gsub(",","",value)) %>%
  mutate(value = as.numeric(value))

b <- t %>% filter(Year=="2017") %>% rename(Base=value) %>% select(-Year)
h <- t %>% filter(Year=="2045") %>% rename(Future=value) %>% select(-Year)

pop.emp <- left_join(b,h,by=c("name")) %>%
  mutate(Delta=Future-Base) %>%
  mutate(Ratio=Delta/Base) %>%
  mutate(LAGR=Delta/(Base*(2045-2017))) %>%
  mutate(CAGR=((Future/Base)^(1/(2045-2017)))-1)

county.pop.cagr <- pop.emp %>% filter(name=="Tot Pop") %>% select(CAGR) %>% pull
county.pop.lagr <- pop.emp %>% filter(name=="Tot Pop") %>% select(LAGR) %>% pull
county.job.cagr <- pop.emp %>% filter(name=="Total Jobs") %>% select(CAGR) %>% pull
county.job.lagr <- pop.emp %>% filter(name=="Total Jobs") %>% select(LAGR) %>% pull

# Transit Ridership -------------------------------------------------------
transit.outputs <- NULL
for (run in model.runs) {
  a <- as_tibble(fread(file.path(getwd(),paste0("/data/",run,"/daily_boardings_by_agency.csv")))) %>%
    filter(agency_name=="Pierce Transit") %>%
    mutate(model_run=run) %>%
    rename(name=agency_name)
  
  r <- as_tibble(fread(file.path(getwd(),paste0("/data/",run,"/transit_line_results.csv")))) %>%
    filter(description %in% c("1 6th Ave / Pacific","BRT LPA-Hybrid10-min")) %>%
    select(description,boardings) %>%
    group_by(description) %>%
    summarize(boardings=sum(boardings)) %>%
    rename(name=description) %>%
    mutate(name="Pacific Avenue BRT") %>%
    mutate(model_run=run)
  
  a <- bind_rows(a,r)
  
  ifelse(is.null(transit.outputs), transit.outputs <- a, transit.outputs <- bind_rows(transit.outputs, a))
  
}

base <- transit.outputs %>% filter(model_run=="base" & name == "Pierce Transit") %>% select(boardings) %>% pull()
future <- transit.outputs %>% filter(model_run=="plan" & name == "Pierce Transit") %>% select(boardings) %>% pull()
agency.cagr <- ((future/base) ^(1/32))-1

base <- transit.outputs %>% filter(model_run=="base" & name == "Pacific Avenue BRT") %>% select(boardings) %>% pull()
future <- transit.outputs %>% filter(model_run=="plan" & name == "Pacific Avenue BRT") %>% select(boardings) %>% pull()
brt.cagr <- ((future/base) ^(1/yrs))-1

# TAZ Corridor Growth -----------------------------------------------------
corridor.pop.base <- taz.data %>% filter(model_run=="base") %>% select(`Total-Population`) %>% pull() %>% sum() 
corridor.pop.plan <- taz.data %>% filter(model_run=="plan") %>% select(`Total-Population`) %>% pull() %>% sum() 
corridor.pop.cagr <- ((corridor.pop.plan/corridor.pop.base) ^(1/32))-1

corridor.emp.base <- taz.data %>% filter(model_run=="base") %>% select(`Total-Jobs`) %>% pull() %>% sum() 
corridor.emp.plan <- taz.data %>% filter(model_run=="plan") %>% select(`Total-Jobs`) %>% pull() %>% sum() 
corridor.emp.cagr <- ((corridor.emp.plan/corridor.emp.base) ^(1/32))-1

# Summary Growth Table ----------------------------------------------------
tbl <- data.table(`Metric` = c("Pierce County Population", "Pierce County Jobs", 
                               "Pacific Avenue Corridor Population", "Pacific Avenue Corridor Jobs",
                               "Pacific Avenue", "Combined Screenline (no Canyon Road)", "Combined Screenline (with Canyon Road)", "I-5", 
                               "Pacific Avenue s/o 121st", "Combined Screenline (no Canyon Road) s/o 121st", "Combined Screenline (with Canyon Road)  s/o 121st",
                               "Pacific Avenue n/o 121st", "Combined Screenline (no Canyon Road) n/o 121st", "Combined Screenline (with Canyon Road)  n/o 121st",
                               "East/West south of 121st", "East/West north of 121st", 
                               "Pierce Transit", "Pacific Avenue BRT"),
                  `Compound Annual Growth Rate` = c(county.pop.cagr, county.job.cagr, 
                                                    corridor.pop.cagr, corridor.emp.cagr,
                                                    sr7.corridors.cagr, corridors.no.cany.cagr, corridors.cagr, i5.corridors.cagr,
                                                    sr7.south.cagr, corridors.south.no.cany.cagr, corridors.south.cagr,
                                                    sr7.north.cagr, corridors.north.no.cany.cagr, corridors.north.cagr,
                                                    ew.south.cagr, ew.north.cagr,
                                                    agency.cagr, brt.cagr)) %>%
  mutate(across(where(is.numeric), label_percent(accuracy = 0.1)))

growth.summary.table <- kbl(tbl, caption = paste0("Average Annual Growth Rates: PSRC 2022-2050 RTP"), booktabs = T, align='lc') %>%
  kable_styling(latex_options = "striped") %>%
  kable_styling(latex_options = "scale_down") %>%
  pack_rows("Population & Employment", 1, 4) %>%
  pack_rows("Vehicle Miles Traveled: SR 7 Entire Corridor", 5, 8) %>%
  pack_rows("Vehicle Miles Traveled: SR 7 South of 121st", 9, 11) %>%
  pack_rows("Vehicle Miles Traveled: SR 7 121st to 38th", 12, 14) %>%
  pack_rows("Vehicle Miles Traveled: East/West Facilities", 15, 16) %>%
  pack_rows("Transit Ridership", 17, 18) %>%
  footnote(general = paste0("Source: PSRC Regional Transportation Plan Analysis"), general_title = " ")
