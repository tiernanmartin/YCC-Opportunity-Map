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



# Load data ----


url <- "https://data.seattle.gov/resource/y7pv-r3kh.json"

# d <- read.socrata(url) 

# Convert to sf
d_sf <- 
  d %>% 
  as_tibble() %>% 
  mutate(latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude)) %>% 
  st_as_sf(coords = c("longitude","latitude")) %>% 
  st_set_crs(4326)
  


all <- 
  d_sf %>% 
  st_drop_geometry() %>% 
  mutate(
    TRACT = str_extract(census_tract_2000, "^.{2}"),
    CID_LGL = if_else(TRACT %in% c("91","92"),TRUE,FALSE),
    ALL = TRUE,
    CODE = as.numeric(summary_offense_code),
    DESC = summarized_offense_description,
    CODE_FCT = fct_explicit_na(factor(CODE),"Other"),
    DESC_FCT_CODE = fct_reorder(DESC,CODE,fun = first),
    DESC_FCT_N = fct_infreq(factor(DESC),TRUE),
    DESC_FCT_CODE_10 = fct_lump(DESC_FCT_CODE,n = 9,other_level = "OTHER")
  ) %>% 
  gather(GRP,GRP_LGL, CID_LGL, ALL) %>% 
  select(matches("GRP|TRACT|CID|CODE|DESC",ignore.case = FALSE), everything()) %>% 
  group_by(GRP, GRP_LGL, DESC_FCT_CODE_10) %>% 
  summarise(CNT = n()) %>% 
  filter(as.character(DESC_FCT_CODE_10) %!in% "OTHER") %>% 
  arrange(desc(CNT)) %>% 
  slice(1:5)
  
  

# filter to CID tracts and 2017
cid_2017_sf <- 
  d_sf %>% 
  mutate(tract = str_extract(census_tract_2000, "^.{2}")) %>% 
  filter(tract %in% c("91","92"),
         date_reported >= anytime("20170101"))

# Munging ----

cid <-   
  cid_2017_sf %>% 
  transmute(
    code = as.numeric(summary_offense_code),
    desc = summarized_offense_description,
    code_fct = fct_explicit_na(factor(code),"Other"),
    desc_fct_code = fct_reorder(desc,code,fun = first),
    desc_fct_n = fct_infreq(factor(desc),TRUE),
    desc_fct_code_10 = fct_lump(desc_fct_code,n = 9,other_level = "OTHER")
  ) 

# Charts ----

d2 <- 
  d %>% 
  st_drop_geometry() %>% 
  group_by(desc_fct_code) %>% 
  mutate(ALL_CNT = n()) %>% 
  ungroup %>% 
  mutate(IS_CID - if_else())

gg <- ggplot(data = d, mapping = aes(desc_fct_code_10))
gg <- gg + geom_histogram()


# Maps ----

pal <- colorFactor("Set3",domain = cid$desc_fct_code_10)

leaflet(cid) %>% 
  addProviderTiles(providers$Stamen.TonerLite) %>% 
  addCircleMarkers(
    popup = ~ desc_fct_code_10,
    color = ~ pal(desc_fct_code_10),
    fillColor = ~pal(desc_fct_code_10),
    clusterOptions = markerClusterOptions
  )

