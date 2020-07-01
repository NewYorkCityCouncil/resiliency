library(raster)
library(tidyverse)
library(sf)
library(spatialEco)
library(leaflet)
library(rgdal)
library(data.table)
library(htmlwidgets)


# QUICK LOAD FILES ------------------------------------------------------------


kde_heat <- raster("data/output/kde_heatmap.tif")
kde_heat_crop <- raster("data/output/kde_heatmap_cropped.tif")
median_temp_sf <- read_sf("data/output/median_satellite_surface_temperatures.shp")
median_temp_sp <- as(median_temp_sf, "Spatial")
nyc1 <-read_sf("data/output/nyc_custom_shapefile/nyc_custom_shapefile.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
load("data/output/AC_map_sf.RData")





# Residential Heatmap Cropping --------------------------------------------


# source: https://www1.nyc.gov/site/planning/data-maps/open-data.page
# NYC GIS Zoning Features

zoning <- read_sf("data/input/nycgiszoningfeatures_202003shp/nyzd.shp") %>% 
  janitor::clean_names() %>% 
  mutate(zoning = ifelse(str_detect(zonedist, "PARK"),
                         "Park",
                         ifelse(str_detect(zonedist, "PLAYGROUND"),
                                "Playground",
                                ifelse(str_detect(zonedist, "C"),
                                       "Commercial",
                                       ifelse(str_detect(zonedist, "^R[1-9]"),
                                              "Residential",
                                              ifelse(str_detect(zonedist, "M"),
                                                     "Manufacturing",
                                                     "Unknown")))))) %>% 
  st_transform("+proj=longlat +datum=WGS84")

residential <- zoning %>% 
  filter(zoning == "Residential")


#crop & mask the raster files to poylgon extent/boundary
res_heat_masked <- mask(kde_heat_crop, residential)
res_heat_cropped <- crop(res_heat_masked, residential)




# Create Open Spaces Shapefile --------------------------------------------


places_list <- "Cemetery|State park/forest|Airport or airfield|Open space|School or academy|Amusement center|Golf course|County park/forest|Other park/forst|City park/forest|Campground|National Park Service land|National Forest/other Fed land|Private park/forest|Town park/forest"

green_spaces <- read_sf("data/input/State_Parks_Airports_Prisons/Parks_Airports_Prisons.shp") %>%
  janitor::clean_names() %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  filter(grepl(places_list, type))

green_mat = st_intersects(green_spaces, nyc1, sparse = FALSE)
green_city_list <- apply(green_mat, 1, any)

green_spaces$incity <- green_city_list

nyc_green_spaces <- green_spaces %>% 
  filter(incity == TRUE) %>% 
  mutate(name = ifelse(!is.na(name),
                       name,
                       type)) %>% 
  select(type, name, geometry)


# Source: https://data.cityofnewyork.us/Recreation/Open-Space-Parks-/g84h-jbjm
parks_and_playgrounds <- read_sf("data/input/Open Space (Parks)/geo_export_9cdc0d3f-3551-49bc-8d18-41aa3d30ffbe.shp") %>% 
  janitor::clean_names() %>% 
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(landuse <- ifelse(!is.na(landuse),
                           landuse,
                           system)) %>% 
  rename(name = park_name, type = landuse) %>% 
  select(type, name, geometry)


## Combine all Open Spaces
all_open_spaces <- rbind(nyc_green_spaces, parks_and_playgrounds)

st_write(all_open_spaces, "data/output/all_open_spaces/all_open_spaces.shp", layer = ".shp", delete_layer = TRUE)




# Relevant Comorbidities --------------------------------------------------


## Add Relevant Comorbidities
relevant_comorbidities <- read_csv("data/input/CDC_Hypertension_COPD.csv") %>% 
  janitor::clean_names()

relevant_comorbidities <- as.data.table(relevant_comorbidities)
relevant_comorbidities[,tract := as.character(as.numeric(stringr::str_sub(tractfips,-6,-1))/100)] #extract census tract number from tractfips
relevant_comorbidities[,boro_code := stringr::str_sub(tractfips,-8,-7)]
relevant_comorbidities[tract == "170.1",tract := "170.10"] #Census Tract 170.1 is coded as '170.10' in our shapefile
Convertboro <- data.table(Census_boro_code = c('05','47','81','85','61'), 
                          boro_code=c(2,3,4,5,1), 
                          boro_name = c("Bronx","Brooklyn","Queens","Staten Island","Manhattan"))
relevant_comorbidities[,c("boro_code","boro_name") := .(sapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_code]),
                                                  sapply(boro_code, function(x) Convertboro[Census_boro_code == x,boro_name]))]

relevant_comorbidities <- as_tibble(relevant_comorbidities)

#add geometry for comorbidities
census_tracts <- read_sf("data/input/2010 Census Tracts/geo_export_43c9ca4f-b641-4766-a769-a6030c0b8ccf.shp") %>% 
  janitor::clean_names() %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  select(boro_ct201, boro_name, boro_code, ctlabel, ntaname, geometry) %>% 
  mutate(boro_code = as.numeric(boro_code)) %>% 
  rename(census_tract = boro_ct201, 
         tract = ctlabel)

relevant_comorbidities <- left_join(census_tracts, relevant_comorbidities)

relevant_comorbidities <- relevant_comorbidities %>% 
  filter(!is.na(intersect_prob))

write_sf(relevant_comorbidities, "data/ouptut/relevant_comorbidities.shp", delete_layer = TRUE)


# Add Covid-19 Open Streets -----------------------------------------------


# Source: https://data.cityofnewyork.us/Health/Open-Streets-Locations/uiay-nctu
open_streets <- read_sf("data/input/Open Streets Locations/geo_export_360ab02d-04b9-4014-915b-3ca883d8c3e8.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")











# # Filter for hotspots
# heat_sf <- median_temp_sf %>% 
#   filter(zscore >= 2)
# 
# # shows what percentage of data we'll be using; flexible, depending on how much
# # you want to see.
# nrow(heat_sf)/nrow(median_temp_sf)



# Palettes & Values -----------------------------------------------------------


pal_rev <- rev(colorRamps::matlab.like(15))
heat_pal <- colorNumeric(rev(colorRamps::matlab.like(15)), domain = values(kde_heat_crop),
                         na.color = "transparent")

legend_val <- seq(min(median_temp_sp$zscore), max(median_temp_sp$zscore), by = 1)
legend_pal <- colorNumeric(colorRamps::matlab.like(length(legend_val)), domain = legend_val)


parkstreets_pal <- c("green", "grey")
parkstreets_val <- c("Parks/Green Spaces", "Open Street Locations")

ac_pal <- colorBin("RdYlBu", domain = ac_map_sf$Percent.of.Households)

comorbidity_pal <- colorBin("Oranges", domain = relevant_comorbidities$intersect_prob)


# Popups ------------------------------------------------------------------



open_spaces_pop <- paste0("Open Space: ", all_open_spaces$name)

streets_pop <- paste0("Street: ", open_streets$on_street, "<br>",
                      "Between: ", open_streets$from_stree, " and ", open_streets$to_street, "<br>",
                      "Days Open: ", open_streets$day_of_wee, "<br>",
                      "Start Time: ",open_streets$start_time, "<br>",
                      "End Time: ", open_streets$end_time)

ac_pop <- paste0("Households with AC: ", round(ac_map_sf$Percent.of.Households, 2), "%", "<br>",
                "PUMA: ", ac_map_sf$puma, "<br>",
                "Boro: ", ac_map_sf$Borough, "<br>",
                "Neighborhood: ", ac_map_sf$SubBoroNam)

comorbidity_pop <- paste0("Census Tract: ", relevant_comorbidities$census_tract, "<br>",
                          "Neigbhorhood: ", relevant_comorbidities$ntaname, "<br>",
                          "% population with hypertension/COPD: ", relevant_comorbidities$intersect_prob)


# Mapping -----------------------------------------------------------------


# Maps / Arguments:
#   Target More Open Space Where its Hot
# Target More AC Distribution Where People are Sick and Hot
# 
# MAP 1 - Where We Need More Open Space 
# Layers:
#   Heat (More is Bad)
# Open Space (More is Good)
# Open Streets (More is Good)
# 
# MAP 2 - Where AC Distribution is Needed 
# Layers:
#   Heat (More is Bad)
# Comorbity (More is Bad)
# AC Access (More is Good)


map1 <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4, group = "Citywide") %>% 
  addRasterImage(res_heat_cropped, colors = heat_pal, opacity = 0.4, group = "Residential Only") %>% 
  addPolygons(data = all_open_spaces, weight = .5, popup = ~open_spaces_pop, fillColor = "green",color = "green", group = "Parks/Green Spaces") %>% 
  addPolygons(data = open_streets, weight = 4, popup = ~streets_pop, color = "grey", fillColor = "grey", group = "Open Street Locations") %>% 
  addLegend(position = "bottomleft", colors = parkstreets_pal, labels = parkstreets_val, title = "Open Spaces", group = "Parks/Green Spaces") %>% 
  addLegend(position = "topleft", pal = legend_pal, values = legend_val, title = paste0("Temperature Deviation", "<br>", "from Mean"),  labFormat = labelFormat(prefix = " ")) %>% 
  addLayersControl(baseGroups = c("Citywide", "Residential Only"),options = layersControlOptions(collapsed = FALSE), position = "bottomright")

map1

withr::with_dir('images', saveWidget(map1, file="open_space_heat_kde.html"))


map2 <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
  addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4, group = "Heat Map") %>% 
  #addRasterImage(res_heat_cropped, colors = heat_pal, opacity = 0.4, group = "Residential Only") %>% 
  addPolygons(data = ac_map_sf, weight = .5, color = "grey", fillColor = ac_pal(ac_map_sf$Percent.of.Households), fillOpacity = 0.5, label = lapply(ac_pop,HTML), group = "Air Conditioning Access") %>% 
  addPolygons(data = relevant_comorbidities, weight = .5, color = "grey", fillColor = comorbidity_pal(relevant_comorbidities$intersect_prob), fillOpacity = .5, group = "Comorbidities") %>% 
  addLegend(position = "topleft", pal = legend_pal, values = legend_val, title = paste0("Temperature Deviation", "<br>", "from Mean"),  labFormat = labelFormat(prefix = " ")) %>% 
  addLegend(position = "topleft", pal = comorbidity_pal, values = relevant_comorbidities$intersect_prob, group = "Comorbidities", title = paste0("Percent Residents", "<br>", "with Hypertension/COPD")) %>%
  addLegend(position = 'topleft', pal = ac_pal, values = ac_map_sf$Percent.of.Households, group = "Air Conditioning Access", title = "Percent Households with AC")  %>% 
  addLayersControl(overlayGroups = c("Heat Map", "Air Conditioning Access", "Comorbidities"),
                   options = layersControlOptions(collapsed = FALSE), position = "bottomright") %>% 
  hideGroup(c("Air Conditioning Access", "Comorbidities"))

map2

withr::with_dir('images', saveWidget(map2, file="ac_comorb_heat_kde.html"))




# heatmap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 10, maxZoom = 16)) %>%
#   addProviderTiles('CartoDB.Positron', options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
#   addRasterImage(kde_heat_crop, colors = heat_pal, opacity = 0.4, group = "Citywide") %>% 
#   addRasterImage(res_heat_cropped, colors = heat_pal, opacity = 0.4, group = "Residential Only") %>% 
#   addPolygons(data = all_open_spaces, weight = .5, popup = ~open_spaces_pop, fillColor = "green",color = "green", group = "Parks/Green Spaces") %>% 
#   addPolygons(data = open_streets, weight = 4, popup = ~streets_pop, color = "grey", fillColor = "grey", group = "Open Street Locations") %>% 
#   addPolygons(data = ac_map_sf, weight = .5, color = "grey", fillColor = ac_pal(ac_map_sf$Percent.of.Households), fillOpacity = 0.5, label = lapply(ac_pop,HTML), group = "Air Conditioning Access") %>% 
#   addPolygons(data = relevant_comorbidities, weight = .5, color = "grey", fillColor = comorbidity_pal(relevant_comorbidities$intersect_prob), fillOpacity = .5, group = "Comorbidities") %>% 
#   #addLegend(position = "bottomleft", colors = parkstreets_pal, labels = parkstreets_val, title = "Open Spaces", group = "Parks/Green Spaces") %>% 
#   addLegend(position = "topleft", pal = legend_pal, values = legend_val, title = paste0("Temperature Deviation", "<br>", "from Mean"),  labFormat = labelFormat(prefix = " ")) %>% 
#   addLegend(position = 'bottomleft', pal = ac_pal, values = ac_map_sf$Percent.of.Households, group = "Air Conditioning Access", title = "Percent Households with AC")  %>% 
#   addLegend(position = "bottomleft", pal = comorbidity_pal, values = relevant_comorbidities$intersect_prob, group = "Comorbidities", title = "Percent Residents with Hypertension/COPD") %>% 
#   addLayersControl(baseGroups = c("Citywide", "Residential Only"),
#                    overlayGroups = c("Parks/Green Spaces", "Open Street Locations", "Air Conditioning Access", "Comorbidities"),
#                    options = layersControlOptions(collapsed = FALSE), position = "bottomright") %>% 
#   hideGroup(c("Open Street Locations", "Air Conditioning Access", "Comorbidities"))
# 
# 
# heatmap

# identify hot spots that are residential, for example exclude the bklyn navy yard and airports or maybe add neighborhood labels too (edited) 
# open streets should be a touch darker

