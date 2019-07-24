library(velox)
library(raster)
library(leaflet)
library(sf)


str_name<-'LC08_CU_029007_20190526_20190607_C01_V01_ST/LC08_CU_029007_20190526_20190607_C01_V01_ST.tif' 
st_raw=raster(str_name)

str(st_raw)
plot(st_raw)

#read-in the polygon shapefile
blocks <- read_sf("2010 Census Blocks/geo_export_2a55aded-68ac-4081-866e-5d019b8fe0f7.shp") %>% 
  st_transform(., crs(st_raw))

tracts <- read_sf('2010 Census Tracts/geo_export_43c9ca4f-b641-4766-a769-a6030c0b8ccf.shp') %>% 
  st_transform(., crs(st_raw))

vx <- velox(st_raw)

temps <- vx$extract(sp=blocks$geometry)

blocks$temps <- temps
blocks$mean_temp <- sapply(blocks$temps, mean)
#blocks$mean_temp <- blocks$mean_temp/100
blocks_for_csv <- blocks
blocks_for_csv$temps <- NULL
blocks_for_csv$geometry <- NULL

#only about 1% of data is missing, will use gams to impute missingness
sum(is.na(blocks$mean_temp))

write_csv(blocks_for_csv, 'block_temps.csv')




temps_tract <- vx$extract(sp=tracts$geometry)
tracts$temps <- temps_tract
tracts$mean_temp <- sapply(tracts$temps, mean)
tracts$max_temp <- sapply(tracts$temps, max)
tracts$min_temp <- sapply(tracts$temps, min)
tracts$mean_temp <- tracts$mean_temp/100
tracts$mean_temp <- tracts$max_temp/100
tracts$mean_temp <- tracts$min_temp/100
tracts_for_csv <- tracts


tracts_for_csv$temps <- NULL
tracts_for_csv$geometry <- NULL

#only about 1% of data is missing, will use gams to impute missingness
sum(is.na(blocks$mean_temp))

write_csv(tracts_for_csv, 'tract_temps.csv')


#map tract level


ar_norm_sta_pal <- colorQuantile(
  palette = "Purples",
  domain = ship_norm$sta_norm_ar,
  n = 5)

ar_norm_pop <- paste0(ar_norm_res_frt_tr_pop, "<br>", ar_norm_comm_frt_svc_pop,
                      "<br>", ar_norm_sta_pop, "<br>", ar_norm_tot_pop)

leaflet(tracts) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addPolygons(fillColor = ~ar_norm_res_frt_tr_pal(ship_norm$res_frt_tr_norm_ar),
              weight = 0,
              group = 'Residential',
              fillOpacity = .9,
              popup = ~ar_norm_pop) %>% 



mean(temps[[1]])
mean(blocks$temps[[1]])
temps[[1]]
