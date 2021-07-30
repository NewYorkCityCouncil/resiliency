### NYC Council Data Team
### Zip Code Covid Only 

rm(list=ls())

### Libraries

library(readr)
library(data.table)
library(stringr)
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(dplyr)

### Import and Clean Covid and Crosswalk Data

# Covid data from NYC Health
URL_C19 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv"
C19 <- fread(URL_C19) 
C19$MODIFIED_ZCTA <- as.character(C19$MODIFIED_ZCTA)

# Taken from historical commits (8/31/20)
# https://github.com/nychealth/coronavirus-data/commits/master?after=e0967ab7350b1024a05accc1794d5c336cb404fb+454&branch=master
URL_C19_Aug31 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/bd0a5fe2dd4e768856b64b1d17b295793ae2488f/data-by-modzcta.csv"
C19_Aug31 <- fread(URL_C19_Aug31) 
C19_Aug31$MODIFIED_ZCTA <- as.character(C19_Aug31$MODIFIED_ZCTA)

# Taken from historical commits (6/1/20)
# https://github.com/nychealth/coronavirus-data/commits/master?after=e0967ab7350b1024a05accc1794d5c336cb404fb+559&branch=master
URL_C19_Jun1 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/53f5d7935feb50fe87132c928f45455af4149407/data-by-modzcta.csv"
C19_Jun1 <- fread(URL_C19_Jun1) %>%
  mutate(TOTAL_COVID_TESTS = NA)
C19_Jun1$MODIFIED_ZCTA <- as.character(C19_Jun1$MODIFIED_ZCTA)

# Combine into "summer"
C19_Summer <- merge(C19_Aug31[,c(1,2,3,5)], C19_Jun1[,c(1,5)], by = "MODIFIED_ZCTA")
C19_Summer$COVID_CASE_RATE <- (C19_Summer$COVID_CASE_RATE.x + C19_Summer$COVID_CASE_RATE.y) / 2

# write.csv(C19, "data/input/current_covid.csv", row.names = FALSE)
# write.csv(C19_Aug31, "data/input/Aug31_covid.csv", row.names = FALSE)
# write.csv(C19_Jun1, "data/input/Jun1_covid.csv", row.names = FALSE)
# write.csv(C19_Summer[,c(1:3,6)], "data/input/Summer_covid.csv", row.names = FALSE)

# https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources
nyczipjson <- read_sf("Data/MODZCTA_2010/MODZCTA_2010.shp") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  rename(MODIFIED_ZCTA = MODZCTA)

map_sf_zip <- st_sf(merge(nyczipjson,C19, by = "MODIFIED_ZCTA"))
map_sf_zip_Aug31 <- st_sf(merge(nyczipjson,C19_Aug31, by = "MODIFIED_ZCTA"))
map_sf_zip_Jun1 <- st_sf(merge(nyczipjson,C19_Jun1, by = "MODIFIED_ZCTA"))
map_sf_zip_Summer <- st_sf(merge(nyczipjson,C19_Summer[,c(1:3,6)], by = "MODIFIED_ZCTA"))













labels_All <- paste("<h3>","COVID19 Death Rate: ",round(map_sf_zip$COVID_DEATH_RATE, 2), "</h3>",
                    "<p>",paste0("Zip Code: ",map_sf_zip$MODZCTA),"</p>",
                    "<p>","Boro: ",map_sf_zip$BOROUGH_GROUP,"</p>",
                    "<p>","Neighborhood: ",map_sf_zip$NEIGHBORHOOD_NAME,"</p>")


map <- leaflet(options = options(viewer = NULL)) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=map_sf_zip,
              weight = 1,
              color = "grey",
              group="COVID19 Death Rate",
              fillColor = ~colorBin("YlOrRd", domain = map_sf_zip$COVID_DEATH_RATE)(map_sf_zip$COVID_DEATH_RATE),
              fillOpacity = 0.5,
              label = lapply(labels_All,HTML))


