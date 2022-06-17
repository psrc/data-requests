# Packages ----------------------------------------------------------------

# Data Wrangling
library(tidyverse)
library(data.table)

# General Inputs ----------------------------------------------------------
model.runs <- c("base", "plan")

corridor.sections <- list(list("pac_Ave_38th_to_11th.txt","SR7-3"),
                          list("yakima_38th_to_11th.txt","Yakima-3"),
                          list("portland_38th_to_11th.txt","Portland-3"))

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

# Corridor Summaries ------------------------------------------------------
yrs <- 2040 - 2018

# All Corridors
all.corridors.base <- link.outputs %>% filter(!(is.na(corridor))) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
all.corridors.future <- link.outputs %>% filter(!(is.na(corridor))) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
all.corridors.growth <- (all.corridors.future - all.corridors.base) / all.corridors.base

all.corridors.lagr <- ((all.corridors.future - all.corridors.base) / (all.corridors.base*yrs))
all.corridors.cagr <- ((all.corridors.future/all.corridors.base) ^(1/yrs))-1

# SR 7
sr7.corridors.base <- link.outputs %>% filter(corridor %in% c("SR7-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
sr7.corridors.future <- link.outputs %>% filter(corridor %in% c("SR7-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
sr7.corridors.growth <- (sr7.corridors.future - sr7.corridors.base) / sr7.corridors.base

sr7.corridors.lagr <- ((sr7.corridors.future - sr7.corridors.base) / (sr7.corridors.base*yrs))
sr7.corridors.cagr <- ((sr7.corridors.future/sr7.corridors.base) ^(1/yrs))-1

# Yakima Avenue
yak.corridors.base <- link.outputs %>% filter(corridor %in% c("Yakima-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
yak.corridors.future <- link.outputs %>% filter(corridor %in% c("Yakima-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
yak.corridors.growth <- (yak.corridors.future - yak.corridors.base) / yak.corridors.base

yak.corridors.lagr <- ((yak.corridors.future - yak.corridors.base) / (yak.corridors.base*yrs))
yak.corridors.cagr <- ((yak.corridors.future/yak.corridors.base) ^(1/yrs))-1

# Portland Avenue
port.corridors.base <- link.outputs %>% filter(corridor %in% c("Portland-3")) %>% filter(model_run=="base") %>% select(vmt) %>% pull() %>% sum()
port.corridors.future <- link.outputs %>% filter(corridor %in% c("Portland-3")) %>% filter(model_run=="plan") %>% select(vmt) %>% pull() %>% sum()
port.corridors.growth <- (port.corridors.future - port.corridors.base) / port.corridors.base

port.corridors.lagr <- ((port.corridors.future - port.corridors.base) / (port.corridors.base*yrs))
port.corridors.cagr <- ((port.corridors.future/port.corridors.base) ^(1/yrs))-1

