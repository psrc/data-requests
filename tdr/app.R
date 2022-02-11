# Packages for Dashboard
library(shiny)
library(shinydashboard)

# Packages for Data Wrangling
library(tidyverse)
library(here)
library(data.table)

# Packages for Maps
library(sf)
library(leaflet)

# Inputs ------------------------------------------------------------
per.capita.rates <- as_tibble(fread(here('data','vmt-per-capita-rgeo.csv')))
data_counties <- per.capita.rates %>% select(County) %>% unique() %>% filter(County != 'Region')
data_rgeo <- per.capita.rates %>% select(Geography) %>% unique() %>% filter(Geography != 'All')
annual.factor <- 320

# User Interface for Dashboard --------------------------------------------
ui <- dashboardPage(skin = "green", title = "Draft TDR Sketch Planning Tools",
                    dashboardHeader(title = "Impact to VMT of Transfer of Development Rights", titleWidth = '25%'),
    
    dashboardSidebar(
        sidebarMenu(
            br(),
            menuItem("Inputs", tabName = "tdr-inputs", icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        tabItems(

            # Inputs
            tabItem(tabName = "tdr-inputs",
                    fluidRow(column(width = 2, 
                                    textInput("totalpersons", "How many People are you Transferring?", value=0),
                                    selectInput("County","County:",data_counties, selected = "Pierce"),
                                    selectInput("CurrentRGeo","What Regional Geography are they coming from?",data_rgeo, selected = "Rural")),
                             column(width=4,
                                    textInput("metroshare", "Share to Metropolitan Cities", value=0),
                                    textInput("coreshare", "Share to Core Cities", value=0),
                                    textInput("hctshare", "Share to High Capacity Transit Communities", value=0),
                                    textInput("citytownshare", "Share to Cities & Towns", value=0),
                                    textInput("uushare", "Share to Urban Unincorporated Places", value=0),
                                    textInput("ruralshare", "Share to Rural Places", value=0)),
                             column(width = 6,
                                    infoBoxOutput("metrobox", width = NULL),
                                    infoBoxOutput("corebox", width = NULL),
                                    infoBoxOutput("hctbox", width = NULL),
                                    infoBoxOutput("citiesbox", width = NULL),
                                    infoBoxOutput("uubox", width = NULL),
                                    infoBoxOutput("ruralbox", width = NULL))),
                    fluidRow(infoBoxOutput("currentvmt", width = NULL)),
                    fluidRow(infoBoxOutput("adjustedvmt", width = NULL)),
                    fluidRow(infoBoxOutput("deltavmt", width = NULL))
            )
        
            
        ) # end of tab items for main body
        
    ) #end of dashboard body
)

#

# Server functions for Dashboard ------------------------------------------

server <- function(input, output) {
    
    new.pop <- reactive({input$totalpersons})
    metro.shr <- reactive({input$metroshare})
    core.shr <- reactive({input$coreshare})
    hct.shr <- reactive({input$hctshare})
    city.shr <- reactive({input$citytownshare})
    uu.shr <- reactive({input$uushare})
    rural.shr <- reactive({input$ruralshare})
    
    current.rate <- reactive({per.capita.rates %>% 
        filter(Geography==input$CurrentRGeo & County==input$County & metric=="daily vmt per capita") %>% 
        select(value) %>% 
        pull()
    })
    
    metro.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='Metropolitan Cities' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    core.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='Core Cities' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    hct.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='High Capacity Transit Communities' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    city.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='Cities and Towns' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    uu.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='Urban Unincorporated' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    rural.rate <- reactive({per.capita.rates %>% 
            filter(Geography=='Rural' & County==input$County & metric=="daily vmt per capita") %>% 
            select(value) %>% 
            pull()
    })
    
    current.vmt <- reactive({round(as.numeric(current.rate())*as.integer(new.pop())*annual.factor,-2)
    })
    
    new.vmt <- reactive({round(as.numeric(metro.rate())*as.integer(new.pop())*as.numeric(metro.shr())*annual.factor+
            as.numeric(core.rate())*as.integer(new.pop())*as.numeric(core.shr())*annual.factor+
            as.numeric(hct.rate())*as.integer(new.pop())*as.numeric(hct.shr())*annual.factor+
            as.numeric(city.rate())*as.integer(new.pop())*as.numeric(city.shr())*annual.factor+
            as.numeric(uu.rate())*as.integer(new.pop())*as.numeric(uu.shr())*annual.factor+
            as.numeric(rural.rate())*as.integer(new.pop())*as.numeric(rural.shr())*annual.factor,-2)
    })
    
    delta.vmt <- reactive({as.numeric(new.vmt())-as.numeric(current.vmt())})
    percent.delta.vmt <- reactive({round(as.numeric(delta.vmt())/as.numeric(current.vmt())*100,0)})
    
    output$metrobox <- renderInfoBox({
        
        infoBox(
            "New Population in Metropolitan Cities:", HTML(paste0(round(as.integer(new.pop())*as.numeric(metro.shr()),-1))), icon = icon("city"),
            color = "green", fill=TRUE
        )
    })
    
    output$corebox <- renderInfoBox({
        
        infoBox(
            "New Population in Core Cities:", HTML(paste0(round(as.integer(new.pop())*as.numeric(core.shr()),-1))), icon = icon("building"),
            color = "blue", fill=TRUE
        )
    })
    
    output$hctbox <- renderInfoBox({
        
        infoBox(
            "New Population in High Capacity Transit Communities:", HTML(paste0(round(as.integer(new.pop())*as.numeric(hct.shr()),-1))), icon = icon("subway"),
            color = "aqua", fill=TRUE
        )
    })
    
    output$citiesbox <- renderInfoBox({
        
        infoBox(
            "New Population in Cities & Towns:", HTML(paste0(round(as.integer(new.pop())*as.numeric(city.shr()),-1))), icon = icon("home"),
            color = "orange", fill=TRUE
        )
    })
    
    output$uubox <- renderInfoBox({
        
        infoBox(
            "New Population in Urban Unincorporated Areas:", HTML(paste0(round(as.integer(new.pop())*as.numeric(uu.shr()),-1))), icon = icon("warehouse"),
            color = "red", fill=TRUE
        )
    })
    
    output$ruralbox <- renderInfoBox({
        
        infoBox(
            "New Population in Rural Areas:", HTML(paste0(round(as.integer(new.pop())*as.numeric(rural.shr()),-1))), icon = icon("tree"),
            color = "black", fill=TRUE
        )
    })
    
    output$currentvmt <- renderInfoBox({
        
        infoBox(
            "Current Annual VMT:", HTML(paste0(format(as.numeric(current.vmt()), nsmall=0, big.mark=","))), icon = icon("car"),
            color = "navy", fill=TRUE
        )
    })
    
    output$adjustedvmt <- renderInfoBox({
        
        infoBox(
            "Projected Annual VMT:", HTML(paste0(format(as.numeric(new.vmt()), nsmall=0, big.mark=","))), icon = icon("car"),
            color = "teal", fill=TRUE
        )
    })
    
    output$deltavmt <- renderInfoBox({
        
        infoBox(
            "Projected Annual Change in VMT:", HTML(paste0(format(as.numeric(delta.vmt()), nsmall=0, big.mark=","),br(),as.integer(percent.delta.vmt()),"%")), icon = icon("car"),
            color = "purple", fill=TRUE
        )
    })
}

shinyApp(ui, server)
