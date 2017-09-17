# Setup -----

# library(scales)
# library(sp)
# library(rgdal)
# library(plotly)
# library(gplots)
# library(ggplot2)
# library(GISTools)
# library(magrittr)
# library(operator.tools)
# library(shiny)
# library(shinydashboard)
# library(DT)
# library(leaflet)
# library(plyr)
# library(knitr)
# library(rprojroot)
# library(rgdal)
# library(sp)
# library(rgeos)
# library(tigris)
# library(leaflet)
# library(ggthemes)
# library(ggrepel)
# library(magrittr)
# library(stringr)
# library(downloader)
# library(webshot)
# library(htmltools)
# library(gplots)
# library(ggmap)
# 
# library(htmlwidgets)
# library(readxl)
# library(acs)
# library(RColorBrewer)
# library(tidyverse)
# library(operator.tools)
# library(ggiraph)
# library(leaflet.extras)
# library(viridisLite)
library(gplots)
library(shiny)
library(shmodules)
library(miscgis)
library(tidyverse)
library(sf)
library(mapview)  
library(leaflet)
library(leaflet.extras)
library(slickR)
library(htmlwidgets)

# root <- rprojroot::is_rstudio_project
# root_file <- root$make_fix_file()

proj_light_grey <- col2hex("grey75")
proj_grey <- col2hex("grey50")
proj_dark_grey <- col2hex("grey25")
proj_orange <- '#D59C40'

# Load data -----

p_sf <- read_rds(root_file('1-data/3-interim/priorities_sf.rds'))


# UI Modules -----



tabItemContentUI_map <- function(id, tab_name){
  
  ns <- NS(id)
  
  tabItem(tabName = tab_name, 
          tags$head(tags$style(HTML("section.content{padding:0px;}
                                    .outer { height: calc(100vh - 50px); padding: 0px; margin: 0; }"))), 
          tags$div(class = "outer", 
                   leafletOutput(ns("map"), 
                                 height = "100%", 
                                 width = "100%")
                   )
          )
}

tabItemContentUI_list <- function(id, tab_name){
 
  ns <- NS(id)
  tabItem(tab_name,
          fluidPage(
            fluidRow(
              box(width = 2, title = "Histogram", status = "primary"),
              box(width = 2, title = "Inputs", status = "warning",
                  "Box content here", br(), "More box content"),
              box(width = 2, title = "Histogram", status = "primary"),
              box(width = 6,title = "Histogram", status = "primary") 
            )
            
          )
          
  )
}

tabItemContentUI_table <- function(id, tab_name){
    
  ns <- NS(id)
  
  tabItem(tab_name,
          box(title = "Table", status = "primary")
          
  )

}

tabItemContentUI_documents <- function(id, tab_name){
  
  ns <- NS(id)
  tabItem(tab_name,
          box(title = "Docs", status = "primary")
          
  )
}

tabItemContentUI_about <- function(id, tab_name){
  
  ns <- NS(id)
  tabItem(tab_name,
          fluidPage(
            fluidRow(
              column(3),
              column(6,
                     box(width = 12, title = "About", status = "primary",
                         uiOutput(ns("about")))
                     ),
              column(3)
              
            )
            
          )
          
  )
}


# Server Modules -----

tabItemContent_map <- function(input, output, session){
  
  ns <- session$ns
  
  output$map <- renderLeaflet({ myLflt(tile_opts = list(minZoom = 10),chinatown = TRUE) 
    })
  
}

tabItemContent_about <- function(input, output, session){
  
  ns <- session$ns
  
  content <- includeHTML(root_file("./2-communication/4-shinyapps/markdown/about.html"))
  
  output$about <- renderUI({content})
  
}

