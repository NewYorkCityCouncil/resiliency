library(tidyverse)
library(sf)
library(leaflet)
library(pdftools)
library(ggplot2)



# Curbside Pickup Mapping -------------------------------------------------


# source: https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4
cds <- read_sf("data/input/Community Districts/geo_export_0fe69218-d898-4df2-bdcd-3c26e72e35d9.shp") %>% 
  select(boro_cd, geometry)

# sources: https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/residents/current-organics-rollout,
# https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/2018-organics-rollout
manhattan <- rep(101:112)
bronx <- rep(201:212)
brooklyn <- rep(301:318)
queens <- rep(401:414)
staten_island <- rep(501:503)


yes <- c(208, 210:212, 301, 302, 306, 307, 310:313, 315, 316, 402, 405, 407:411, 413, 414)

enroll <- c(101:112, 201:207, 209)

partial <- 501

partial_text <- "Available in Castleton Corners, Graniteville, Mariners Harbor, Port Richmond, West Brighton, Westerleigh"


curbside_pickup <- tibble(
  boro_cd = c(manhattan, bronx, brooklyn, queens, staten_island)
) %>% 
  mutate(curbside_pickup_status = ifelse(boro_cd %in% yes,
                                         "Available",
                                         ifelse(boro_cd %in% enroll,
                                                "Available with Enrollment",
                                                ifelse(boro_cd == partial,
                                                       partial_text,
                                                       "Not Available"))))

# join and get rid of non-cd geometry
curbside_pickup_shape <- left_join(cds, curbside_pickup) %>% 
  filter(!is.na(curbside_pickup_status)) %>% 
  st_transform('+proj=longlat +datum=WGS84')


pop <- paste0("Community Board: ", curbside_pickup_shape$boro_cd, "<br>",
              "Curbside Pickup: ", curbside_pickup_shape$curbside_pickup_status)

pal <- colorFactor(palette = c("Green", "Yellow","Red", "Orange"),
  levels = unique(curbside_pickup_shape$curbside_pickup_status))



leaflet(curbside_pickup_shape) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = ~pal(curbside_pickup_status), popup = pop) %>% 
  addLegend(pal = pal, values = curbside_pickup_shape$curbside_pickup_status)






# Central Park Precipitation ----------------------------------------------

precip_pdf <- pdf_text('https://www.weather.gov/media/okx/Climate/CentralPark/monthlyannualprecip.pdf') %>% 
  readr::read_lines()

precip_raw <- precip_pdf[c(3:46,49:96,99:146,149:160)] %>% 
  str_squish() %>% 
  strsplit(split = " ")

variables <- precip_raw[1] %>% 
  unlist()

data_lines <- precip_raw[2:152]

precip <- plyr::ldply(data_lines)
colnames(precip) <- variables

annual_precip <- as_tibble(precip) %>% 
  janitor::clean_names() %>% 
  select(year, annual) %>% 
  mutate(year = as.numeric(year),
         annual = as.numeric(annual))

ggplot(annual_precip, aes(x=year, y=annual)) +
  geom_line() +
  ylab("Annual Precipitation (Inches)") +
  xlab("Year")


