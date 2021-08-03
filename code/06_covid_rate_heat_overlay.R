library(raster)
library(tidyverse)
library(sf)
library(spatialEco)
library(leaflet)
library(rgdal)
library(data.table)
library(htmlwidgets)
library(classInt)


# QUICK LOAD FILES ------------------------------------------------------------


kde_heat <- raster("data/output/kde_heatmap.tif")
kde_heat_crop <- raster("data/output/kde_heatmap_cropped.tif")
median_temp_sf <- read_sf("data/output/median_satellite_surface_temperatures.shp")
median_temp_sp <- as(median_temp_sf, "Spatial")
nyc1 <-read_sf("data/output/nyc_custom_shapefile/nyc_custom_shapefile.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
load("data/output/AC_map_sf.RData")



# Load Covid Rate Files ---------------------------------------------------

# Taken from historical commits (8/31/20)
# https://github.com/nychealth/coronavirus-data/commits/master?after=e0967ab7350b1024a05accc1794d5c336cb404fb+454&branch=master
URL_C19_Aug31_20 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/bd0a5fe2dd4e768856b64b1d17b295793ae2488f/data-by-modzcta.csv"
C19_Aug31_20 <- fread(URL_C19_Aug31_20) 
C19_Aug31_20$MODIFIED_ZCTA <- as.character(C19_Aug31_20$MODIFIED_ZCTA)

# Taken from historical commits (6/1/20)
# https://github.com/nychealth/coronavirus-data/commits/master?after=e0967ab7350b1024a05accc1794d5c336cb404fb+559&branch=master
URL_C19_Jun1_20 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/53f5d7935feb50fe87132c928f45455af4149407/data-by-modzcta.csv"
C19_Jun1_20 <- fread(URL_C19_Jun1_20) %>%
  mutate(TOTAL_COVID_TESTS = NA)
C19_Jun1_20$MODIFIED_ZCTA <- as.character(C19_Jun1_20$MODIFIED_ZCTA)

# Taken from historical commits (6/1/21)
# https://github.com/nychealth/coronavirus-data/tree/713cfee574d265e40ced1d4426f01f82367a44c0
URL_C19_Jun1_21 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/713cfee574d265e40ced1d4426f01f82367a44c0/totals/data-by-modzcta.csv"
C19_Jun1_21 <- fread(URL_C19_Jun1_21) %>%
  mutate(TOTAL_COVID_TESTS = NA)
C19_Jun1_21$MODIFIED_ZCTA <- as.character(C19_Jun1_21$MODIFIED_ZCTA)


# Taken from historical commits (7/31/20)
# https://github.com/nychealth/coronavirus-data/tree/5a78ed7b254c3997786e98d6776e4ed4f740937b
URL_C19_Jul31_21 <-"https://raw.githubusercontent.com/nychealth/coronavirus-data/5a78ed7b254c3997786e98d6776e4ed4f740937b/totals/data-by-modzcta.csv"
C19_Jul31_21 <- fread(URL_C19_Jul31_21) %>%
  mutate(TOTAL_COVID_TESTS = NA)
C19_Jul31_21$MODIFIED_ZCTA <- as.character(C19_Jul31_21$MODIFIED_ZCTA)



# Calculate average monthly case rate over these periods ------------------


# divide by three for average over June, July, August
summ_20_covid <-left_join(C19_Aug31_20[,c(1,2,5)], C19_Jun1_20[,c(1,5)], by = "MODIFIED_ZCTA", suffix = c("aug", "jun")) %>% 
  mutate(monthly_covid_case_rate = round((COVID_CASE_RATEaug - COVID_CASE_RATEjun)/3)) %>% 
  select(-COVID_CASE_RATEaug, -COVID_CASE_RATEjun)

# divide by two for average over June, July
summ_21_covid <- left_join(C19_Jul31_21[,c(1,8)], C19_Jun1_21[,c(1,8)], by = "MODIFIED_ZCTA", suffix = c("jul", "jun")) %>% 
  mutate(monthly_covid_case_rate = round((COVID_CASE_RATEjul - COVID_CASE_RATEjun)/2)) %>% 
  select(-COVID_CASE_RATEjul, -COVID_CASE_RATEjun)

# divide by 13 for average over June 20, Jul 20, Aug, Sept, Nov, Dec 20, Jan 21, Feb, Mar, Apr, May, Jun 21, Jul 21
jun_20_present_covid <- left_join(C19_Jul31_21[,c(1,8)], C19_Jun1_20[,c(1,5)], by = "MODIFIED_ZCTA", suffix = c("jul21", "jun20")) %>% 
  mutate(monthly_covid_case_rate = round((COVID_CASE_RATEjul21 - COVID_CASE_RATEjun20)/13)) %>% 
  select(-COVID_CASE_RATEjul21, -COVID_CASE_RATEjun20)



# all sets
all_covid_periods <- left_join(summ_20_covid, summ_21_covid, 
                               by = "MODIFIED_ZCTA", suffix = c("_summ_20", "_summ_21")) %>% 
  left_join(., jun_20_present_covid, by = "MODIFIED_ZCTA") %>% 
  rename(monthly_covid_case_rate_summ_20_present = monthly_covid_case_rate)


# Add ZCTA Shapefile ------------------------------------------------------


nyczipjson <- read_sf("data/input/MODZCTA_2010/MODZCTA_2010.shp") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  rename(MODIFIED_ZCTA = MODZCTA)

zip_covid <- st_sf(merge(nyczipjson, all_covid_periods, by = "MODIFIED_ZCTA")) %>% janitor::clean_names()



# Case Rate Descriptive Stats/Breaks --------------------------------------


all_rate_values <- c(zip_covid$monthly_covid_case_rate_summ_20, 
                     zip_covid$monthly_covid_case_rate_summ_21, 
                     zip_covid$monthly_covid_case_rate_summ_20_present)

all_rates <- tibble(all_rate_values)

min = min(all_rates)
max = max(all_rates)
diff <- max - min
std = sd(all_rates$all_rate_values)

equal.interval = seq(min, max, by = diff/6)
quantile.interval = quantile(all_rates$all_rate_values, probs=seq(0, 1, by = 1/6))
std.interval = c(seq(min, max, by=std), max)
natural.interval = classIntervals(all_rates$all_rate_values, n = 6, style = 'jenks')$brks

case_rate_equal = cut(all_rate_values, breaks=equal.interval, include.lowest = TRUE)
case_rate_quantile = cut(all_rate_values, breaks=quantile.interval, include.lowest = TRUE)
case_rate_std = cut(all_rate_values, breaks=std.interval, include.lowest = TRUE)
case_rate_natural = cut(all_rate_values, breaks=natural.interval, include.lowest = TRUE)



ggplot(all_rates, aes(x = all_rate_values)) + geom_histogram()
ggplot(all_rates, aes(x=all_rate_values)) + geom_histogram(breaks=equal.interval, stat='bin')
ggplot(all_rates, aes(x=all_rate_values)) + geom_vline(aes(xintercept=all_rate_values), color='red') + geom_histogram(breaks=equal.interval)






# Palettes & Values -----------------------------------------------------------


# heat map palette
pal_rev <- rev(colorRamps::matlab.like(15))
heat_pal <- colorNumeric(rev(colorRamps::matlab.like(15)), domain = values(kde_heat_crop),
                         na.color = "transparent")





# covid case rate palette
# let's start by combining case rates 

case_rate_pal <- colorBin("Oranges", bins = round(natural.interval), domain = all_rates$all_rate_values)



# Map ---------------------------------------------------------------------

covid_heat_overlay <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4, group = "Surface Tempterature Map") %>% 
  addPolygons(data = zip_covid, weight = 1, fillOpacity = .8, 
              fillColor = case_rate_pal(zip_covid$monthly_covid_case_rate_summ_20_present),
              group = "June 1 2020 to July 31 2021 Cases") %>% 
  addPolygons(data = zip_covid, weight = 1, fillOpacity = .8, 
              fillColor = case_rate_pal(zip_covid$monthly_covid_case_rate_summ_20),
              group = "June 1 2020 to August 31 2021 Cases") %>% 
  addPolygons(data = zip_covid, weight = 1, fillOpacity = .8, 
              fillColor = case_rate_pal(zip_covid$monthly_covid_case_rate_summ_21),
              group = "June 1 2021 to July 31 2021 Cases") %>% 
  addLayersControl(baseGroups = c("Surface Tempterature Map",
                                  "June 1 2020 to July 31 2021 Cases", 
                                  "June 1 2020 to August 31 2021 Cases",
                                  "June 1 2021 to July 31 2021 Cases"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") %>% 
  addLegend(pal = case_rate_pal,
            values = natural.interval,
            position = "topleft",
            opacity = .8,
            title = "Average Monthly Case Rate")
  

covid_heat_overlay






# # Only Summer 2020 --------------------------------------------------------
# 
# 
# 
# 
# 
# covid_heat_overlay <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)) %>%
#   addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
#   addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4, group = "Surface Tempterature Map") %>% 
#   addPolygons(data = zip_covid, weight = 1, 
#               fillColor = case_rate_pal(zip_covid$monthly_covid_case_rate_summ_20),
#               group = "Summer 2020 Cases") %>% 
#   addLayersControl(baseGroups = c("Surface Tempterature Map",
#                                   "Summer 2020 Cases"),
#                    options = layersControlOptions(collapsed = FALSE),
#                    position = "bottomright")
