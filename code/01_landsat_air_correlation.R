library(tidyverse)
library(velox)
library(raster)
library(sf)
library(leaflet)



# Air Temps ---------------------------------------------------------------


# Central Park
cp_raw <- read_csv("data/input/Ground_Monitor_Temps_NYC/central_park_temp.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(station, date, hourly_dry_bulb_temperature) %>% 
  mutate(name = "Central Park")

#separate date and time out, as we may not be able to use time
hours_cp <- format(as.POSIXct(strptime(cp_raw$date,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")

dates_cp <- format(as.POSIXct(strptime(cp_raw$date,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d")

cp_raw$date <- dates_cp
cp_raw$our <- hours_cp

# La Guardia
lag_raw <- read_csv("data/input/Ground_Monitor_Temps_NYC/laguardia_temp.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(station, date, hourly_dry_bulb_temperature) %>% 
  mutate(name = "La Guardia Airport")

#separate date and time out, as we may not be able to use time

hours_lag <- format(as.POSIXct(strptime(lag_raw$date,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")

dates_lag <- format(as.POSIXct(strptime(lag_raw$date,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d")

lag_raw$date <- dates_lag
lag_raw$hour <- hours_lag




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


raster_test <- raster("data/input/LC08_CU_029007_20140604_20190504_C01_V01_ST.tif")

k_to_f <- function(temp) { fahrenheight <- ((temp - 273) * (9/5)) + 32  }


filenames <- list.files("data/input/landsat_st", pattern="*.tif", full.names=TRUE)
test_filenames = filenames[1:3]
ldf <- lapply(test_filenames, raster)
res <- lapply(ldf, summary)
names(res) <- substr(test_filenames, 6, 30)


#read in temperature rasters
raster_read <- function(file, string) {
  name <- paste0('heat/lidar_rasters/', string, '.tif')
  filename <- paste0('rstr_july_', file)
  filename <- raster(name)
  return(filename)
}


temp_func <-function(sf, rastername, stringname) {
  rstr <- velox(rastername)
  sf <- st_transform(sf, crs(rastername))
  temps <- rstr$extract(sp=sf$geometry)
  mean_temp <- paste0('mean_temp_', stringname)
  max_temp <- paste0('max_temp_', stringname)
  min_temp <- paste0('min_temp_', stringname)
  sf[,mean_temp] <- sapply(temps, mean, na.rm = TRUE)
  sf[,mean_temp] <- k_to_f(sf[,mean_temp]/10)
  sf[,max_temp] <- sapply(temps, max)
  sf[,max_temp] <- k_to_f(sf[,max_temp]/10)
  sf[,min_temp] <- sapply(temps, min)
  sf[,min_temp] <- k_to_f(sf[,min_temp]/10)
  sf
}

landsat <- temp_func(shapes, raster_test, 'test')

cp_day_test <- cp_raw %>% 
  filter(date == as.POSIXct("2014-06-04"),
         !is.na(hourly_dry_bulb_temperature))

cp_test_avg <- round(mean(cp_day_test$hourly_dry_bulb_temperature),0)

lag_day_test <- lag_raw %>% 
  filter(date == as.POSIXct("2014-06-04"),
         !is.na(hourly_dry_bulb_temperature))

lag_test_avg <- round(mean(lag_day_test$hourly_dry_bulb_temperature),0)

air_avg <- c(cp_test_avg, lag_test_avg)

landsat$air_avg <- air_avg
