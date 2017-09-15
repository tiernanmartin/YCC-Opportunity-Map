# Setup ----

library(magrittr)
library(tidyverse)
library(sf)
library(RSocrata)
library(leaflet)
library(stringr)
library(lubridate)
library(anytime)
library(forcats)
library(googlesheets)    # devtools::install_github('jennybc/googlesheets')
library(googledrive)     # devtools::install_github('tidyverse/googledrive')
library(mapedit)         # devtools::install_github("r-spatial/mapedit")
library(mapview)         # devtools::install_github("r-spatial/mapview@develop")
library(leaflet.extras)  # devtools::install_github("bhaskarvk/leaflet.extras")
library(snakecase)
library(shiny)
library(mapview)
library(slickR)
library(htmlwidgets)


options(httr_oob_default=TRUE) 
gs_auth(new_user = TRUE) 
# Load data ----

url <- "https://docs.google.com/spreadsheets/d/1FyQfHytdcwvAgeGvn-1H4Mca1_paMsNsWS6ZbhXGNIA/edit?usp=sharing"

opp_sheet <- gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)

priorities <- 
  opp_sheet %>% 
  gs_read(ws = "Priorities", range = cell_limits(c(5, 2), c(NA, 9)), colnames = TRUE) %>% 
  rename_all(snakecase::to_screaming_snake_case)

images <- 
  opp_sheet %>% 
  gs_read(ws = "Image URLs", colnames = TRUE) %>%  
  rename_all(snakecase::to_screaming_snake_case)


p <- priorities

p$geometry <- st_sfc(purrr::rerun(nrow(p),st_geometrycollection()))

p <- st_as_sf(p)


nc <- st_read(system.file("shape/nc.shp", package="sf")) 


make_an_sf <- function(dat) {
  ui <- fluidPage(
    fluidRow(
      column(6,DT::dataTableOutput("tbl",width="100%", height="400px")),
      column(6,editModUI("map"))
    ),
    fluidRow(actionButton("donebtn", "Done"))
  )
  
  server <- function(input, output, session) {
    data_copy <- st_as_sf(
      dat,
      geometry = st_sfc(lapply(seq_len(nrow(dat)),function(i){st_point()}))
    )
    
    edits <- callModule(
      editMod,
      leafmap = mapview()@map,
      id = "map"
    )
    output$tbl <- DT::renderDataTable({
      DT::datatable(
        dat,
        options = list(scrollY="400px"),
        # could support multi but do single for now
        selection = "single"
      )
    })
    
    # unfortunately I did not implement last functionality
    #  for editMap, so do it the hard way
    # last seems useful, so I might circle back and add that
    EVT_DRAW <- "map_draw_new_feature"
    EVT_EDIT <- "map_draw_edited_features"
    EVT_DELETE <- "map_draw_deleted_features"
    
    nsm <- function(event="", id="map") {
      paste0(session$ns(id), "-", event)
    }
    
    observe({
      possible <- list(
        draw = input[[nsm(EVT_DRAW)]],
        edit = input[[nsm(EVT_EDIT)]],
        delete = input[[nsm(EVT_DELETE)]]
      )
      
      # get last event
      last <- Filter(Negate(is.null), possible)
      # really dislike that we need to do in R
      pos <- Position(Negate(is.null), possible)
      
      # get selected row
      selected <- isolate(input$tbl_rows_selected)
      
      skip = FALSE
      # ignore if selected is null
      #  not great but good enough for poc
      if(is.null(selected)) {skip = TRUE}
      
      # ignore if no event
      if(length(last) == 0) {skip = TRUE}
      
      # replace if draw or edit
      if(skip==FALSE && (names(possible)[pos] %in% c("edit","draw"))) {
        
        sf::st_geometry(data_copy[selected,]) <<- sf::st_geometry(
          mapedit:::st_as_sfc.geo_list(unname(last)[[1]])
        )
      }
      
      # remove if delete
      if(skip==FALSE && (names(possible)[pos] %in% c("delete"))) {
        sf::st_geometry(data_copy[selected,]) <<- sf::st_sfc(st_point())
      }
    })
    
    # provide mechanism to return after all done
    observeEvent(input$donebtn, {
      # convert to sf
      
      stopApp(st_sf(data_copy,crs=4326))
    })
  }
  
  return(runApp(shinyApp(ui,server)))
}

tmp <- make_an_sf(p)

tmp <- read_rds("./1-data/3-interim/priorities_sf.rds")

my_slickr <- function(x){
  slickR(x,  
         height = 300,
         width = '100%',
         padding=0,
         slickOpts = list(
           dots = TRUE,
           arrows = TRUE,
           slidesToShow = 1,
           slidesToScroll = 1
         ) 
  ) 
}

tmp_split <- 
  tmp %>% 
  mutate(POINT_LGL = map_lgl(geometry,~ .x %>% class %>% magrittr::extract(2) %>% str_detect("POINT")),
         LINESTRING_LGL = map_lgl(geometry,~ .x %>% class %>% magrittr::extract(2) %>% str_detect("LINESTRING"))) %>% 
  # left_join(select(images,UNIQUE_ID,URL)) %>% 
  mutate(URL = list(rep("http://gdurl.com/zdcV", times = 2))) %>% 
  mutate(POPUP = map(URL,my_slickr))

tmp_pts <- tmp_split %>% filter(POINT_LGL)
tmp_line <- tmp_split %>% filter(LINESTRING_LGL)



mapview()



pop_test <- slickR(rep(test_url, times = 2),  
                   height = 300,
                   width = '100%',
                   padding=0,
                   slickOpts = list(
                     dots = TRUE,
                     arrows = TRUE,
                     slidesToShow = 1,
                     slidesToScroll = 1
                   ) 
) 


mapview(tmp_pts, 
        popup = popupGraph(tmp_pts$POPUP,type = "html",height = 380, width = 580),
          options = popupOptions(maxWidth = 2000),height = 380, width = 580)

leaflet() %>% 
  addProviderTiles(provider = providers$Stamen) %>% 
  addMarkers(data = tmp_pts, popup = ~NAME) %>% 
  addPolylines(data = tmp_line, popup = ~NAME)


# let's act like breweries does not have geometries
brewsub <- breweries[1:4,1:4,drop=TRUE]

brewsub_2 <- bind_rows(brewsub,brewsub) %>% arrange(brewery)

brewpub_sf <- make_an_sf(brewsub_2)

test <- aggregate(brewpub_sf, list(brewpub_sf$brewery), function(x) x[1])

mapview(brewpub_sf)


