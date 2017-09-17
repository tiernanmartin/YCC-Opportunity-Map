# SETUP -----
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
# # library(rprojroot)
# library(rgdal)
# library(sp)
# library(rgeos)
# library(tigris)
# library(leaflet)
# library(ggthemes)
# library(magrittr)
# library(stringr)
# library(downloader)
# library(webshot)
# library(htmltools)
# library(gplots)
# library(ggmap)
# library(shiny)
# library(htmlwidgets)
# library(readxl)
# library(acs)
# library(RColorBrewer)
# library(tidyverse)
# library(operator.tools)
# library(ggiraph)
# library(leaflet.extras)
# library(viridisLite)
# library(shmodules) #devtools::install_github('tiernanmartin/shmodules')
# library(miscgis)
# library(tidyverse)
# library(sf)
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
library(shinydashboard)
library(rprojroot)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
source(root_file("/2-communication/4-shinyapps/global.R"))

# UI -----

header <- dashboardHeader(title = "CID Opportunity Explorer",titleWidth = "350px")

sidebar <- dashboardSidebar(
  width = "350px",
  shmodules::sidebarCSS(),
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  sidebarMenu(id = 'menu',
              menuItem("Community Priorties & Coming Changes",
                       startExpanded = TRUE,
                       tabName = "CPCC",
                       icon = icon("building", lib = "font-awesome"),
                       menuSubItem("Map",tabName = "map",icon = icon("globe", lib = "font-awesome")), 
                       menuSubItem("Lists",tabName = "lists",icon = icon("list-ul", lib = "font-awesome")), 
                       menuSubItem("Table",tabName = "table",icon = icon("table", lib = "font-awesome")),
                       menuSubItem("Documents", tabName = "documents", icon = icon("book", lib = "font-awesome"))), 
              menuItem("About", tabName = "about", icon = icon("question-circle", lib = "font-awesome"))
  ),
  tags$style(HTML("p{font-size: 12px;}
                        #barmap1-var_text{background-color: #1e282c;margin-top: 15px;}
                        #searchButton{margin:0px;}
                        .sidebar-form {border:0px !important;}
                        .container-fluid {padding:15px !important}")),
  HTML("<hr style='margin: 5px;height:1px;border-width:0;color:#404040;background-color:#404040'>"),
  HTML("<div style='padding-right: 25px;padding-left: 25px;'>"),
  HTML("</div>")
)


body <- shmodules::fluidDashboardBody(sidebarCollapsed = FALSE,
                                      tabItems( 
                                        tabItemContentUI_map(id = "map",tab_name = "map"),
                                        tabItem(tabName = "lists",
                                                fluidPage(
                                                  fluidRow(
                                                    tabItemContentUI_list(id = "priorities", 
                                                                          box_title = "Priorities"),
                                                    tabItemContentUI_list(id = "change_pri", 
                                                                          box_title = "Changes (Private)",
                                                                          box_status = "warning" ),
                                                    tabItemContentUI_list(id = "change_pub", 
                                                                          box_title = "Changes (Public)",
                                                                          box_status = "danger" ),
                                                    tagList(
                                                      box(width = 6, title = "", 
                                                          projectCardUI("list_proj_card")
                                                      )
                                                    ) 
                                                  )
                                                )
                                        ),
                                        tabItemContentUI_table(id = "table",tab_name = "table"),
                                        tabItemContentUI_documents(id = "documents",tab_name = "documents"),
                                        tabItemContentUI_about(id = "about",tab_name = "about")
                                      )
)

ui <- dashboardPage(header,sidebar,body, skin = 'yellow')

# SERVER  ------

server <- function(input, output, session) {
  
  # Map ----
  callModule(module = tabItemContent_map, id = "map") 
  
  # Lists ----
  callModule(module = tabItemContent_list, id = "priorities", choices = pull(p_sf,NAME))
  
  callModule(module = tabItemContent_list, id = "change_pri" )
  
  callModule(module = tabItemContent_list, id = "change_pub") 
  
  # About ----
  callModule(module = tabItemContent_about, id = "about")
  
} 

# RUN -----

shinyApp(ui, server)
