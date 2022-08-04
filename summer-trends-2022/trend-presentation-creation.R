library(officer)
library(magrittr)

# Basic Inputs ------------------------------------------------------------
presentation.title <- "Puget Sound Trends"
committee.name <- "Growth Management Policy Board"
presentation.date <- "September 2022"

my_pres <- read_pptx("trends-psrc-template-v2.pptx")
source("data-processing.R")

# Create Title Slide ------------------------------------------------------
my_pres <- add_slide(my_pres, layout = "Title Slide") 
my_pres <- ph_with(x=my_pres, 
                   value=presentation.title, 
                   location = ph_location_type(type = "ctrTitle"))
my_pres <- ph_with(x=my_pres, 
                   value=committee.name, 
                   location = ph_location_type(type = "subTitle"))
my_pres <- ph_with(x=my_pres, 
                   value=presentation.date, 
                   location = ph_location_type(type = "dt"))

# Save Presentation -------------------------------------------------------
print(my_pres, target = "test.pptx") 








layout_summary(my_pres)
layout_properties (x = my_pres, layout = "Title Slide" )

