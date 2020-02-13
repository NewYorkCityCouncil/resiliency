library(tidyverse)
library(velox)
library(raster)
library(sf)
library(leaflet)
library(XML)
library(methods)
library(lubridate)


# Notes on Data -----------------------------------------------------------

#' Surface Temp data is sourced through LANDSAT 8, provided by USGS Earth Explorer.
#' The satelite orbits the earth vertically, across the poles. It captures roughly 
#' 112 mile (~180 km) wide swaths of the earth at a time, and circumnavigates the 
#' globe every 99 minutes. This also means that it will pass over the same longitude
#' at the same time, +/- 15 minutes. This makes time comparison quite easy.
#' 
#' To ensure no gaps in data, LANDSAT 8 paths overlap slightly. New York City 
#' conveniently falls in the intersection of two paths - columns 13 and 14 of 
#' row 32. This means we get twice as many measurements. A third path, probably
#' column 15, also picks up as New York City on the Earth Explorer site, but
#' actually contains no (or nearly no) NYC geometry. Accordingly, every third
#' will return NA, as it contains no values within the geometry we are examining.
#' 
#' Data Source:
#' https://earthexplorer.usgs.gov/
#' Acquisition Visualization:
#' https://landsat.usgs.gov/landsat_acq
#' NYC Coordinates: 40.7128° N, 74.0060° W
#' L8 Data User's Handbook:
#' https://prd-wret.s3-us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1574_L8_Data_Users_Handbook-v5.0.pdf
#' 
#' 
#' Note on Cloud Coverage:
#' According to the documentation, cloud coverage levels over 65% are considered
#' cloudy. Days with high cloud coverage seem to present clearly inaccurate 
#' results; 9/8/14, for example, has a cloud coverage of 85% and shows Central
#' Park as having an average temperature of -65 F.
#' 
#' While Earth explorer allows users to filter based on cloud coverage (and if
#' repeating this endeavor, I'd encourage you to download from the source while
#' using this filter), because we already downloaded all dates, I will remove
#' days with significant cloud coverage by pulling from the xml data, which con-
#' tains cloud cover information.
#' 
#' 
#' Note on Temperatures:
#' Temperatures are recorded by LANDSAT 8 in Kelvin*10. In order to make them
#' readable to the general public, you'll see I've added a Kelvin to Fahrenheit
#' converter function, and that I divide all values by 10.





# Air Temps ---------------------------------------------------------------


# Central Park
cp_raw <- read_csv("data/input/Ground_Monitor_Temps_NYC/central_park_temp.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(station, date, hourly_dry_bulb_temperature) %>% 
  mutate(name = "Central Park")

cp_raw <- cp_raw %>% 
  filter(!is.na(date))

# La Guardia
lag_raw <- read_csv("data/input/Ground_Monitor_Temps_NYC/laguardia_temp.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(station, date, hourly_dry_bulb_temperature) %>% 
  mutate(name = "La Guardia Airport")

lag_raw <- lag_raw %>% 
  filter(!is.na(date))




# Geometry ----------------------------------------------------------------


# La Guardia
# source: https://data.cityofnewyork.us/City-Government/Airport-Polygon/xfhz-rhsk

lag_shape <- read_sf("data/input/Airport Polygon/geo_export_3743182d-8d50-4636-9732-390060735bb9.shp") %>% 
  filter(name == "La Guardia Airport") %>% 
  dplyr::select(name, geometry)


# Central Park
# source: https://data.cityofnewyork.us/City-Government/Parks-Properties/k2ya-ucmv

cp_shape <- st_read("data/input/Parks Properties/geo_export_544f716b-41c4-43d7-aa7e-32c264ab6fd6.shp") %>% 
  filter(signname == "Central Park") %>% 
  dplyr::select(signname, geometry) %>% 
  rename(name = signname)

shapes <- rbind(lag_shape, cp_shape)



# Landsat Temps -------------------------------------------------------------

# Converting Kelving to Fahrenheit
k_to_f <- function(temp) { fahrenheight <- ((temp - 273) * (9/5)) + 32  }

temp_func_2 <-function(rastername) {
  rstr <- velox(rastername)
  sf <- st_transform(shapes, crs(rastername))
  temps <- rstr$extract(sp=sf$geometry)
  mean_temp <- paste0('mean_temp')
  max_temp <- paste0('max_temp')
  min_temp <- paste0('min_temp')
  sf[,mean_temp] <- sapply(temps, mean, na.rm = TRUE)
  sf[,mean_temp] <- k_to_f(sf[,mean_temp]/10)
  sf[,max_temp] <- sapply(temps, max)
  sf[,max_temp] <- k_to_f(sf[,max_temp]/10)
  sf[,min_temp] <- sapply(temps, min)
  sf[,min_temp] <- k_to_f(sf[,min_temp]/10)
  sf
}

#load in all raster files and apply the temp func to get mean, max and min temperatures
# within the sf geometry

filenames <- list.files("data/input/landsat_st", pattern="*.tif", full.names=TRUE)
ldf_r <- lapply(filenames, raster)
res <- lapply(ldf_r, temp_func_2)

#pull out just the date for the names of each dataframe
dataset_names <- str_extract(str_extract(filenames, pattern ="029007_[0-9]*"), pattern = "_[0-9]*")

# name the dataframes in the list
names(res) <- dataset_names

#join listed dataframes into single dataframe
raster_shape <- bind_rows(res, .id = "column_label") %>% 
  mutate(date = lubridate::ymd(str_replace(column_label, "_", ""))) %>% 
  filter(!is.na(max_temp))

# a lot of values are coming up as n/a - every third value, in fact.

# After looking at the raster images themselves, I believe it's because, while
# they are categorized under NYC, the satelite doesn't actually pass over much 
# of the city, if any. But they still count it as a NYC file. These will be 
# removed from the analysis
# 
# 
# raster_test <- raster("data/input/landsat_st/LC08_CU_029007_20140908_20190503_C01_V01_ST.tif")
# raster_test <- raster("data/input/landsat_st/LC08_CU_029007_20140604_20190504_C01_V01_ST.tif")
# plot(raster_test)
# rstr <- velox(raster_test)
# sf <- st_transform(shapes, crs(raster_test))
# temps <- rstr$extract(sp=sf$geometry)
# sf$mean <- sapply(temps, mean, na.rm = TRUE)
# sf$mean_temp <- k_to_f(sf$mean/10)
# sf$max_temp <- sapply(temps, max)
# sf$max_f <- k_to_f(sf$max_temp/10)







# XML Cloud Cover Extraction ----------------------------------------------



#' After examining the documentation and the metadata provided in the xml files,
#' it's clear that the the satellite orbits the earth with such consistency (99 
#' minutes) that we don't actually need to include the time - the satelite will
#' always reach row 32 of its orbit (the row in which NYC resides) at ~ 15:30, 
#' +/- 15 minutes. Therefore, for all dates, we'll look at the air temperatures 
#' at 15:00 and 16:00.

# Read in all xmlfiles, and use TreeParse to separate the files into subsettable
# parts
xmlfilenames <- list.files("data/input/landsat_xml", pattern="*.xml", full.names=TRUE)
xmldf <- lapply(xmlfilenames, xmlTreeParse)
xmldf <- lapply(xmldf, xmlRoot)



#as.POSIXct(trimws(str_replace_all(xmlValue(xmldf[[4]][["tile_metadata"]][["global_metadata"]][["aquisition"]][[1]]), "[A-Z]", " ")))

# # Extract all times
# for (i in 1:length(xmldf)) {
#      print(str_replace_all(xmlValue(xmldf[[i]][["scene_metadata"]][["global_metadata"]][["scene_center_time"]][[1]]), "\\.[0-9A-Z]*", ""))
# }

# Extract date and cloud cover information
cloud_list <- list()
for (i in 1:length(xmldf)) {
  date <- xmlValue(xmldf[[i]][["tile_metadata"]][["global_metadata"]][["acquisition_date"]])
  tmp <- list(cloud_cover = xmlValue(xmldf[[i]][["tile_metadata"]][["global_metadata"]][["cloud_cover"]]))
  cloud_list[[date]] <- tmp
}

#transform to dataframe so we can join it with the raster dataframe
cloud_data  <-  as.data.frame(x = list(
  date = ymd(names(cloud_list)), 
  cloud_cover = as.numeric(matrix(unlist(cloud_list)))), 
  stringsAsFactors = FALSE)


#' This allows us to see which raster files both cover New York effectively 
#' (and therefore have valid raster values), and which have reasonably low 
#' cloud coverage, which, based on a discussion with a NASA-affiliated expert, 
#' we'll at below 10. Also based on this discussion, we'll attempt to limit
#' what data we actually use to the last year or two, if possible.
raster_with_metadata <- left_join(cloud_data, raster_shape) %>% 
  filter(cloud_cover < 10,
         !is.na(mean_temp))




# Usage Conclusion --------------------------------------------------------

#' will use 9/22/2019, 8/30/2019 and possibly 7/10/2018 for mapping. See notes
#' at top section of 02_mapping_rasters for reasoning.




# Prepare Air Temps for Join ----------------------------------------------



## Aggregate the air temps by day
cp_dates <- cp_raw %>%  
  filter(!is.na(hourly_dry_bulb_temperature),
         format(date,format='%Y-%m-%d') %in% as.character(raster_shape$date),
         as.POSIXct(format(date,format='%H:%M:%S'), format = "%H:%M:%S")
         %within%
             interval(
               as.POSIXct(paste0(today("EST"), "15:00:00"), 
                          format = "%Y-%m-%d %H:%M:%S", tz = "EST"), 
               as.POSIXct(paste0(today("EST"), "16:00:00"), 
                          format = "%Y-%m-%d %H:%M:%S", tz = "EST")))
                                                                       
  
cp_avg <- aggregate(list(avg_ground_temp = cp_dates$hourly_dry_bulb_temperature),
                    by = list(date = as.POSIXct(format(cp_dates$date,format='%Y-%m-%d')),
                              name = cp_dates$name),
                    function(x) {round(mean(x),0)})



lag_dates <- lag_raw %>%  
  filter(!is.na(hourly_dry_bulb_temperature),
         format(date,format='%Y-%m-%d') %in% as.character(raster_shape$date),
         as.POSIXct(format(date,format='%H:%M:%S'), format = "%H:%M:%S")
         %within%
           interval(
             as.POSIXct(paste0(today("EST"), "15:00:00"), 
                        format = "%Y-%m-%d %H:%M:%S", tz = "EST"), 
             as.POSIXct(paste0(today("EST"), "16:00:00"), 
                        format = "%Y-%m-%d %H:%M:%S", tz = "EST")))

lag_avg <- aggregate(list(avg_ground_temp = lag_dates$hourly_dry_bulb_temperature),
                    by = list(date = as.POSIXct(format(lag_dates$date,format='%Y-%m-%d')),
                              name = lag_dates$name),
                    function(x) {round(mean(x),0)})

air_avg <- bind_rows(cp_avg, lag_avg) %>%
  mutate(date = (ymd(date)))

shape_temps <- dplyr::left_join(air_avg, raster_shape) %>% 
  mutate(colum_label = NULL)

