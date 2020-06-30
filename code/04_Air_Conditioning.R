### Air Conditioning Map
### Households reporting air conditioning (2017)

### Links
# Other Data files:
#https://www.census.gov/data/datasets/2017/demo/nychvs/microdata.html
#https://www2.census.gov/geo/docs/reference/puma/2010_PUMA_Names.txt


rm(list=ls())
library(stringr)
library(sf)
library(readxl)
library(leaflet)
library(htmltools)

# Load AC data and subset to sub-borough / PUMA
#http://a816-dohbesp.nyc.gov/IndicatorPublic/VisualizationData.aspx?id=2185,719b87,107,Summarize
AC <- read.csv("data/input/Households reporting air conditioning.csv", skip=7, header=TRUE)
AC_PUMA <- subset(AC, str_detect(GeoTypeName, "Neighborhood")==TRUE)

# Load data with PUMA and sub-borough names (need data with sub-borough names and PUMA number)
#https://www.baruch.cuny.edu/confluence/display/geoportal/NYC+Geographies (NYC PUMAs and Neighborhoods)
boro_names <- read_xls("data/input/nyc_puma_neighborhood.xls")
boro_names$PUMA <- substr(boro_names$PUMA5CE,2,5)

# Name discrepencies between data sets (e.g. S. instead of South), so do quick and dirty fix
AC_PUMA$NameCompare <- str_replace(substring(AC_PUMA$Geography, 17), "/", " / ")
AC_PUMA$SubBoroNam <- sort(boro_names$SubBoroNam)

# Merge correct sub-borough names with AC data
AC_names <- merge(AC_PUMA, boro_names[,c("SubBoroNam","PUMA")], by="SubBoroNam") %>% 
  rename(puma = PUMA)

# Load shapefile for PUMAs
#https://data.cityofnewyork.us/Housing-Development/Public-Use-Microdata-Areas-PUMA-/cwiz-gcty 
nyc_puma <- read_sf("data/input/Public Use Microdata Areas (PUMA)/geo_export_6a72b7b7-56ee-4fb0-9284-1297f6ac9197.shp") %>%
                           st_transform("+proj=longlat +datum=WGS84")

# Combine shapefile and AC data using PUMA number
ac_map_sf <- st_sf(merge(nyc_puma, AC_names, by = "puma"))
save(ac_map_sf, file = "data/output/AC_map_sf.RData")

write_sf(ac_map_sf, "data/output/ac_map/ac_map.sf", driver = "ESRI Shapefile")


# Map it! 

ac_pal <- colorBin("RdYlBu", domain = map_sf$Percent.of.Households)

ac_pop <- paste("<h3>","Households with AC: ",round(map_sf$Percent.of.Households, 2), "%", sep="", "</h3>",
                "<p>","PUMA: ",map_sf$PUMA,"</p>",
                "<p>","Boro: ",map_sf$Borough,"</p>",
                "<p>","Sub-Borough: ",map_sf$SubBoroNam,"</p>")

leaflet(map_sf) %>%
  setView(-73.935242,40.730610,10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              fillColor = pal(map_sf$Percent.of.Households),
              fillOpacity = 0.5,
              label = lapply(labels,HTML))%>%
  addLegend(title = "Households with AC (%)",
            pal = pal, 
            value = ~Percent.of.Households)


