library(velox)
library(raster)
library(leaflet)
library(sf)



rstr_may=raster('LC08_CU_029007_20190526_20190607_C01_V01_ST/LC08_CU_029007_20190526_20190607_C01_V01_ST.tif')

str(rstr_may)

rstr_july <- raster('LC08_CU_029007_20190713_20190723_C01_V01_ST.tif')

#read-in the polygon shapefile
blocks <- read_sf("2010 Census Blocks/geo_export_2a55aded-68ac-4081-866e-5d019b8fe0f7.shp") %>% 
  st_transform(., crs(rstr_may))

# tracts <- read_sf('2010 Census Tracts/geo_export_43c9ca4f-b641-4766-a769-a6030c0b8ccf.shp') %>% 
#   st_transform(., crs(rstr_may))



temp_func <-function(sf, rastername, stringname) {
  rstr <- velox(rastername)
  st_transform(sf, crs(rastername))
  temps <- rstr$extract(sp=sf$geometry)
  mean_temp <- paste0('mean_temp_', stringname)
  max_temp <- paste0('max_temp_', stringname)
  min_temp <- paste0('min_temp_', stringname)
  sf[,mean_temp] <- sapply(temps, mean)
  sf[,mean_temp] <- sf[,mean_temp]/100
  sf[,max_temp] <- sapply(temps, max)
  sf[,max_temp] <- sf[,max_temp]/100
  sf[,min_temp] <- sapply(temps, min)
  sf[,min_temp] <- sf[,min_temp]/100
  sf
}

test <- blocks

blocks <- temp_func(blocks, rstr_may, 'may')
blocks <- temp_func(blocks, rstr_july, 'july')

blocks <- blocks %>% 
  mutate(., may_quantile_rank = ntile(blocks$mean_temp_may,5))
blocks <- blocks %>% 
  mutate(., july_quantile_rank = ntile(blocks$mean_temp_july,5))



nrow(blocks[blocks$may_quantile_rank != blocks$july_quantile_rank,])


blocks <- st_transform(blocks, crs = 4326)

#remove list columns for csv output
blocks_for_csv <- blocks
blocks_for_csv$temps <- NULL
blocks_for_csv$geometry <- NULL

#only about 1% of data is missing, will use gams to impute missingness
#updated code shows 0, will check in later to confirm
sum(is.na(blocks$mean_temp_may))
sum(is.na(blocks$mean_temp_july))

write_csv(blocks_for_csv, 'block_temps.csv')


#map tract level


blocks_may_heat_pal <- colorQuantile(
  palette = "Oranges",
  domain = blocks$mean_temp_may,
  n = 5)

blocks_july_heat_pal <- colorQuantile(
  palette = "Oranges",
  domain = blocks$mean_temp_july,
  n = 5)

blocks_pop_may <- paste0('Mean Temperature: ', blocks$mean_temp_may, "<br>",
                         'Max Temperature: ', blocks$max_temp_may, "<br>",
                         'Min Temperature: ', blocks$min_temp_may, "<br>")

blocks_pop_july <- paste0('Mean Temperature: ', blocks$mean_temp_july, "<br>",
                          'Max Temperature: ', blocks$max_temp_july, "<br>",
                          'Min Temperature: ', blocks$min_temp_july, "<br>")

m <- leaflet(blocks) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(fillColor = ~blocks_may_heat_pal(blocks$mean_temp_may),
              weight = 0,
              fillOpacity = .9,
              popup = ~blocks_pop_may,
              group = 'May') %>% 
  addPolygons(fillColor = ~blocks_july_heat_pal(blocks$mean_temp_july),
              weight = 0,
              fillOpacity = .9,
              popup = ~blocks_pop_july,
              group = 'July') %>%
  addLegend('topleft', pal = blocks_may_heat_pal,
            values = blocks$mean_temp_may,
            title = 'Census Blocks by Quintile of Mean Surface Temperature') %>% 
  addLayersControl(baseGroups = c('May', 'July'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'bottomright')

m












