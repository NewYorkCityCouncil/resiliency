# Notes -------------------------------------------------------------------

#' After discussion with a NASA-affiliated expert, we've determined that 
#' actually presenting the true surface temperature, regardless of whether it's
#' measured in Kelvin, Celsius or Fahrenheit, will not be very informative for
#' us or end users; what does it mean if the surface temperature is 95 degrees F
#' on a summer day? Is that hold or cold? Additionally, relying on exact temper-
#' atures increases the likelihood of inaccuracy due to the susceptibility of
#' the data to cloud coverage and other factors that obscure satelite access to
#' the ground.
#' 
#' However, as the experit pointed out, while temperatures may fluctuate, and 
#' are susceptible to "memory" (i.e. yesterday's rain may result in cooler sur-
#' face temperatures than expected, even on a scorching day), they nonetheless
#' operate consistently across space; the parts of the city that are the warmest
#' today are still going to be the parts of the city that are warmest tomorrow.
#' This consistency allows us to look at just a handful of the clearest days to
#' get an accurate impression not of temperature, but of relative temperature - 
#' how the temperatures compare to each other.

library(raster)
library(rgdal) # for spTransform
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(spatialEco)
options(scipen = 999)
#below 3% cloud coverage


july_10_18_tif <- 'data/input/landsat_final_used_values/LC08_CU_029007_20180710_20190614_C01_V01_ST.tif'
#0.3% cloud coverage
august_30_19_tif <- 'data/input/landsat_final_used_values/LC08_CU_029007_20190830_20190919_C01_V01_ST.tif'
sept_22_19_tif <- 'data/input/landsat_final_used_values/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif'
nyc <-st_read("data/input/Borough Boundaries/geo_export_c9b00a06-66dd-495d-a6b3-a4e923b39c1b.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")


# Create data
august_30_19_raster <- raster(august_30_19_tif)
july_10_18_raster <- raster(july_10_18_tif)
sept_22_19_raster <- raster(sept_22_19_tif)



#reproject nyc polygon to raster projection
nyc1 <- st_transform(nyc, projection(august_30_19_raster))


#crop & mask the raster files to poylgon extent/boundary
august_30_19_masked <- mask(august_30_19_raster, nyc1)
august_30_19_cropped <- crop(august_30_19_masked, nyc1)

july_10_18_masked <- mask(august_30_19_raster, nyc1)
july_10_18_cropped <- crop(july_10_18_masked, nyc1)

sept_22_19_masked <- mask(august_30_19_raster, nyc1)
sept_22_19_cropped <- crop(sept_22_19_masked, nyc1)



# Convert raster to Sf
august_30_19_sf <- rasterToPoints(august_30_19_cropped, spatial = TRUE) %>%
  as_tibble() %>% 
  mutate(date = mdy("08-30-2019"))


august_30_19_poly <- rasterToPolygons(august_30_19_cropped, dissolve = TRUE)
august_30_19_poly_smooth <- smooth(august_30_19_poly, method = "ksmooth")
plot(august_30_19_poly)
august_30_19_poly_smooth %>%


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

median_temp_sf = median_temp_sf %>% st_set_crs(crs(august_30_19_cropped)) %>% st_transform(4326)

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



# Heat Map ----------------------------------------------------------------


#convert to spatial for sp.kde
median_temp_sp <- as(median_temp_sf, "Spatial")

# Use kernel density estimate (kde) to create heatmap of city; using higher
# row/column values for a resolution that better fits the scale of the data.
kde_heat <- sp.kde(x = median_temp_sp, y = median_temp_sp$zscore,  
        nr = 600, nc = 600)
plot(kde_heat)


# crop this new raster to nyc
nyc1 <- st_transform(nyc, projection(kde_heat))


#crop & mask the raster files to poylgon extent/boundary
kde_heat_masked <- mask(kde_heat, nyc1)
kde_heat_crop <- crop(kde_heat_masked, nyc1)



# Leaflet Map -------------------------------------------------------------


# Filter for hotspots
heat_sf <- median_temp_sf %>% 
  filter(zscore >= 2)

# shows what percentage of data we'll be using; flexible, depending on how much
# you want to see.
nrow(heat_sf)/nrow(median_temp_sf)

pal_rev <- rev(colorRamps::matlab.like(15))
heat_pal <- colorNumeric(rev(colorRamps::matlab.like(15)), values(kde_heat_crop),
                    na.color = "transparent")


leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4) 

%>% 
  addLegend(pal = pal, values = values(r),
          title = "Surface temp")
