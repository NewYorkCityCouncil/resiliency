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

# reload files
kde_heat <- raster("data/kde_heatmap.tif")
kde_heat_crop <- raster("data/kde_heatmap_cropped.tif")
median_temp_sf <- read_sf("data/median_satellite_surface_temperature/median_satellite_surface_temperatures.shp")
median_temp_sp <- as(median_temp_sf, "Spatial")
bk1 <-read_sf("data/bk/nyc_custom_shapefile.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")




# Palettes & Values -----------------------------------------------------------


pal_rev <- rev(colorRamps::matlab.like(15))
heat_pal <- colorNumeric(rev(colorRamps::matlab.like(15)), domain = values(kde_heat_crop),
                         na.color = "transparent")

legend_val <- seq(min(median_temp_sp$zscore), max(median_temp_sp$zscore), by = 1)
legend_pal <- colorNumeric(colorRamps::matlab.like(length(legend_val)), domain = legend_val)



# Mapping -----------------------------------------------------------------






map <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 11, maxZoom = 16)) %>%
  #addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4) #%>% 
 # addLegend(position = "bottomleft", colors = heat_pal, labels = legend_pal) %>% 
  #addLegend(position = "topleft", pal = legend_pal, values = legend_val, title = paste0("Temperature Deviation", "<br>", "from Mean"),  labFormat = labelFormat(prefix = " "))



map



