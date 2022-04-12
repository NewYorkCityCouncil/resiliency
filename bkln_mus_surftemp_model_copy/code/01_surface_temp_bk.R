
# Objective ---------------------------------------------------------------

#' Recreating the surface temperature map previously done for the entire city - 
#' this time, specifically for Brooklyn. This is being done as part of a factual
#' and artistic collaboration with the Brooklyn Museum, led by Mona Chalabi.
#' 
#' Original work (and notes) can be found here: 
#' https://github.com/NewYorkCityCouncil/resiliency



# Libraries and Prep ------------------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ONE OR MORE OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c('raster',
                      'rgdal', # for spTransform
                      'sf',
                      'tidyverse',
                      'lubridate',
                      'leaflet',
                      'ggplot2',
                      'spatialEco',
                      'htmlwidgets')


# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)


options(scipen = 999)


#'  load data - sourced from:
#'  https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-level-2-provisional-surface?qt-science_center_objects=0#qt-science_center_objects
#'  Council readme explains getting access and can be found here:
#'  https://github.com/NewYorkCityCouncil/resiliency

 
#  only using data below 3% cloud coverage for accurate results


july_10_18_tif <- 'data/landsat_final_used_values/LC08_CU_029007_20180710_20190614_C01_V01_ST.tif'
august_30_19_tif <- 'data/landsat_final_used_values/LC08_CU_029007_20190830_20190919_C01_V01_ST.tif'
sept_22_19_tif <- 'data/landsat_final_used_values/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif'


boro <- st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")%>% 
  st_transform("+proj=longlat +datum=WGS84")

bkln <- boro %>% 
  filter(boro_name == "Brooklyn")



# Create data
august_30_19_raster <- raster(august_30_19_tif)
july_10_18_raster <- raster(july_10_18_tif)
sept_22_19_raster <- raster(sept_22_19_tif)


#reproject brooklyn polygon to raster projection
bkln1 <- st_transform(bkln, projection(august_30_19_raster))



#crop & mask the raster files to poylgon extent/boundary
august_30_19_masked <- mask(august_30_19_raster, bkln1)
august_30_19_cropped <- crop(august_30_19_masked, bkln1)

july_10_18_masked <- mask(august_30_19_raster, bkln1)
july_10_18_cropped <- crop(july_10_18_masked, bkln1)

sept_22_19_masked <- mask(august_30_19_raster, bkln1)
sept_22_19_cropped <- crop(sept_22_19_masked, bkln1)




# Convert raster to Sf
august_30_19_sf <- rasterToPoints(august_30_19_cropped, spatial = TRUE) %>%
  as_tibble() %>% 
  mutate(date = mdy("08-30-2019"))


july_10_18_sf <- rasterToPoints(july_10_18_cropped, spatial = TRUE) %>%
  as_tibble() %>% 
  mutate(date = mdy("07-10-2018"))

sept_22_19_sf <- rasterToPoints(sept_22_19_cropped, spatial = TRUE) %>%
  as_tibble() %>% 
  mutate(date = mdy("09-22-2019"))



# Converting Kelving to Fahrenheit
k_to_f <- function(temp) { fahrenheight <- ((temp - 273) * (9/5)) + 32  }



# Merge sfs

collected_sf <- rbind(august_30_19_sf, july_10_18_sf, sept_22_19_sf) %>% 
  mutate(coords = paste0(as.character(x),", ", as.character(y)))



median_temp <- collected_sf %>% 
  rename(temp = LC08_CU_029007_20190830_20190919_C01_V01_ST) %>% 
  group_by(coords) %>% 
  summarise(median_temp = k_to_f(median(temp)/10)) %>% 
  separate(coords, into = c("x", "y"), sep = ", ") %>% 
  mutate(x = as.numeric(x),
         y = as.numeric(y))

median_temp_sf <- st_as_sf(median_temp, coords = c("x", "y")) 

median_temp_sf <- median_temp_sf %>% 
  st_set_crs(crs(august_30_19_cropped)) %>% 
  st_transform(4326)



# As seen below, distribution of points seems pretty normal, slight tail on the 
# left, or possibly even an overlapping of two distributions, driven by var-
# iables about which we don't have access to information.
ggplot(median_temp_sf, aes(x = median_temp)) +
  geom_histogram()




# Z-Scores
#' because distribution is relatively normal, we're electing to go with z-score
#' calculation, so as to represent the distribution accurately without relying
#' on Fahrenheit values

# (value - mean)/stdev

median_temp_sf$zscore <- scale(median_temp_sf$median_temp)

# export median temp shapefile
st_write(median_temp_sf, 'data/median_satellite_surface_temperature/median_satellite_surface_temperatures.shp')






# Heat Map ----------------------------------------------------------------


#convert to spatial for sp.kde
median_temp_sp <- as(median_temp_sf, "Spatial")

start_time <- Sys.time()

# Use kernel density estimate (kde) to create heatmap of city; using higher
# row/column values for a resolution that better fits the scale of the data.
kde_heat <- sp.kde(x = median_temp_sp, y = median_temp_sp$zscore,  
                   nr = 1200, nc = 1200, standardize = TRUE)
plot(kde_heat)


#write output
writeRaster(kde_heat, filename="data/kde_heatmap.tif", format = "GTiff", overwrite=TRUE)

# crop this new raster to nyc
bkln1 <- st_transform(bkln, projection(kde_heat))

write_sf(bkln1, "data/bk/nyc_custom_shapefile.shp")


#crop & mask the raster files to poylgon extent/boundary
kde_heat_masked <- mask(kde_heat, bkln1)
kde_heat_crop <- crop(kde_heat_masked, bkln1)

#write nyc-cropped output
writeRaster(kde_heat_crop, filename="data/kde_heatmap_cropped.tif", format = "GTiff", overwrite=TRUE)





