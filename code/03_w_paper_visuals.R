library(tidyverse)
library(sf)
library(leaflet)
library(pdftools)
library(ggplot2)
library(ggpubr)
library(RSocrata)
library(hrbrthemes)
library(spatialEco)
library(data.table)



# Curbside Pickup Mapping -------------------------------------------------


# source: https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4
cds <- read_sf("data/input/Community Districts/geo_export_0fe69218-d898-4df2-bdcd-3c26e72e35d9.shp") %>% 
  select(boro_cd, geometry)

# sources: https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/residents/current-organics-rollout,
# https://www1.nyc.gov/assets/dsny/site/services/food-scraps-and-yard-waste-page/2018-organics-rollout
manhattan <- rep(101:112)
bronx <- rep(201:212)
brooklyn <- rep(301:318)
queens <- rep(401:414)
staten_island <- rep(501:503)


yes <- c(208, 210:212, 301, 302, 306, 307, 310:313, 315, 316, 402, 405, 407:411, 413, 414)

enroll <- c(101:112, 201:207, 209)

partial <- 501

partial_text <- "Partial Availability" #"Available in Castleton Corners, Graniteville, Mariners Harbor, Port Richmond, West Brighton, Westerleigh"


curbside_pickup <- tibble(
  boro_cd = c(manhattan, bronx, brooklyn, queens, staten_island)
) %>% 
  mutate(curbside_pickup_status = ifelse(boro_cd %in% yes,
                                         "Available",
                                         ifelse(boro_cd %in% enroll,
                                                "Available with Enrollment",
                                                ifelse(boro_cd == partial,
                                                       partial_text,
                                                       "Not Available"))))

# join and get rid of non-cd geometry
curbside_pickup_shape <- left_join(cds, curbside_pickup) %>% 
  filter(!is.na(curbside_pickup_status)) %>% 
  st_transform('+proj=longlat +datum=WGS84')


curbside_pop <- paste0("Community Board: ", curbside_pickup_shape$boro_cd, "<br>",
              "Curbside Pickup: ", curbside_pickup_shape$curbside_pickup_status)

curbside_pal <- colorFactor(palette = c("#007534", "#1D5FD6","#B63F26", "#846126"),
  levels = unique(curbside_pickup_shape$curbside_pickup_status))



leaflet(curbside_pickup_shape,
        options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = ~curbside_pal(curbside_pickup_status), popup = curbside_pop,
              weight = .8) %>% 
  addLegend(pal = curbside_pal, values = curbside_pickup_shape$curbside_pickup_status,
            position = "topleft")






# Central Park Precipitation ----------------------------------------------

precip_pdf <- pdf_text('https://www.weather.gov/media/okx/Climate/CentralPark/monthlyannualprecip.pdf') %>% 
  readr::read_lines()

precip_raw <- precip_pdf[c(3:46,49:96,99:146,149:160)] %>% 
  str_squish() %>% 
  strsplit(split = " ")

variables <- precip_raw[1] %>% 
  unlist()

data_lines <- precip_raw[2:152]

precip <- plyr::ldply(data_lines)
colnames(precip) <- variables

annual_precip <- as_tibble(precip) %>% 
  janitor::clean_names() %>% 
  select(year, annual) %>% 
  mutate(year = as.numeric(year),
         annual = as.numeric(annual))

ggplot(annual_precip, aes(x=year, y=annual)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("Annual Precipitation (Inches)") +
  xlab("Year") +
  # theme_ipsum(axis_title_just = "mc", 
  #             base_size = 8,
  #             axis_title_size = 11,
  #             axis_text_size = 10) +
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  ggtitle("Global Temperature Rise",
          paste("How much warmer than average the most recent year was globally")) +
  labs(tag = "Figure 2.", caption = "NASA's Goddard Institute for Space Studies (GISS)")

ggsave("Global Temperature Rise.png", plot = p, path = "/Users/romartinez/Desktop/sshfs/rm_sustainability_visuals/visuals/", width = 8.5, height = 5, units = "in", dpi = 300)





# Street Flooding Complaints ----------------------------------------------


complaints_2019 <- read.socrata("https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$limit=9999999999&$where=created_date between '2019-01-01T00:00:00.000' and '2020-01-01T00:00:00.000'")

street_flooding <- complaints_2019 %>% 
  filter(grepl('street flood', tolower(descriptor)))

# round lat/long for normalization - 3 decimal places is up to 110 meters
# Like NPCC viz 2.11, normalizing street flooding complaints by number of 
# complaints relative to a space, as some areas are known to submit more 
# complaints than others. Given 3400 complaints across city, 110 m may be 
# appropriate range
complaints_2019_norm <- complaints_2019 %>% 
  mutate(latitude = round(latitude, 3),
         longitude = round(longitude, 3),
         count = 1,
         street_flooding = ifelse(descriptor == unique(street_flooding$descriptor),
                                  1, 0))


norm_flooding <- aggregate(list(street_flooding_complaints = complaints_2019_norm$street_flooding,
                                total_complaints = complaints_2019_norm$count),
                           by = list(latitude = complaints_2019_norm$latitude,
                                     longitude = complaints_2019_norm$longitude),
                           function(x) {sum(x)})

norm_flooding$flooding_normalized <- norm_flooding$street_flooding_complaints/norm_flooding$total_complaints


xy <- norm_flooding[,c(2,1)]

flood_points <- SpatialPointsDataFrame(coords = xy, data = norm_flooding, proj4string = CRS("+proj=longlat +datum=WGS84"))


# Use kernel density estimate (kde) to create heatmap of city; using higher
# row/column values for a resolution that better fits the scale of the data.
kde_flood <- sp.kde(x = flood_points, y = flood_points$street_flooding_complaints,  
                   nr = 600, nc = 600, mask = TRUE,  bw = .018)
plot(kde_flood)



nyc <-st_read("data/input/Borough Boundaries/geo_export_c9b00a06-66dd-495d-a6b3-a4e923b39c1b.shp") 

# crop this new raster to nyc
nyc2 <- st_transform(nyc, projection(kde_flood))


#crop & mask the raster files to poylgon extent/boundary
kde_flood_masked <- mask(kde_flood, nyc2)
kde_flood_crop <- crop(kde_flood_masked, nyc2)

#color palette for raster map
flood_pal <- colorNumeric("Reds", values(kde_flood_crop),
                         na.color = "transparent")


# color palette for legend, as raster values can't be used for legend
sequence <- seq(min(flood_points$street_flooding_complaints), max(flood_points$street_flooding_complaints), 5)
flood_legend_pal <- colorBin(palette = "Reds", bins = sequence, domain = sequence)


#' as the raster prevents veiwers from seeing the location labels (unless the 
#' opacity is lowered so much that it diminishes the visual effect of the 
#' raster), we'll be adding labels in for neighborhoods, so it's easier to 
#' identify which neighborhoods have the greatest number of complaints
flood_points_sf <- st_as_sf(flood_points) %>% 
  filter(street_flooding_complaints > 0)

#add nta to get neighborhood names
ntas <- read_sf("data/input/Neighborhood Tabulation Areas (NTA)/geo_export_7ed77860-f030-45da-bee8-a25c13fcdea8.shp") %>% 
  select(ntaname, geometry) %>% 
  st_transform(crs = crs(flood_points_sf))


flood_points_sf <- st_join(flood_points_sf, ntas)

flood_points_filtered <- as_tibble(flood_points_sf) %>% 
  filter(street_flooding_complaints > 8) %>% 
  distinct(ntaname, .keep_all = TRUE)

flood_points_sf_filtered <- st_as_sf(flood_points_filtered) %>% 
  filter(ntaname != "Lindenwood-Howard Beach")


flood_points_sf_filtered[flood_points_sf_filtered$ntaname ==  "Mariner's Harbor-Arlington-Port Ivory-Graniteville",]$ntaname <- "Mariner's Harbor-Arlington-Port"

#flood_points_sf_filtered[flood_points_sf_filtered$ntaname ==  "Lindenwood-Howard Beach",]$ntaname <- "Lindenwood"

leaflet(flood_points_sf_filtered,
        options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addLabelOnlyMarkers(label = ~ntaname, labelOptions = labelOptions(noHide = T, direction = "Bottom", textOnly = T)) %>% 
  addRasterImage(kde_flood_crop, colors = flood_pal, opacity = 0.8) %>% 
  addLegend(title = "Number of Complaints", 
            pal = flood_legend_pal, 
            values = sequence,
            position = "topleft")



# Waste Characterization Analysis ------------------------------------------





# trash_pdf <- pdf_text("data/input/2017 NYC Waste Characterization Study p16.pdf") %>% 
#   readr::read_lines()
# 
# waste_raw <- trash_pdf[3:19] %>% 
#   trimws(which = "both") %>% 
#   strsplit(split = "\\s{2,}")
# 
# waste_variables <- waste_raw[1] %>% 
#   unlist()
# 
# waste_variables <- c("Material", waste_variables)
# 
# waste_data_lines <- waste_raw[c(3:4, 6:8, 10:13, 15:17)]
# 
# waste_categories <- waste_raw[c(2,5,9,14)] %>% 
#   unlist() 
# 
# waste_categories <- waste_categories[c(1,5,9,13)]
# 
# # number of items for each category: 2, 3, 4, 2
# 
# category <- c(rep(waste_categories[1], 2),
#               rep(waste_categories[2], 3),
#               rep(waste_categories[3], 4),
#               rep(waste_categories[4], 2))
# 
# waste_df <- plyr::ldply(waste_data_lines)
# colnames(waste_df) <- waste_variables
# 
# waste <- as_tibble(waste_df) %>% 
#   janitor::clean_names() %>% 
#   select(material, x2017) %>% 
#   mutate(x2017 = as.numeric(gsub("%", "", x2017))) %>% 
#   filter(!is.na(x2017)) %>% 
#   mutate(category = category,
#          share = paste0(x2017, "%"))




## After speaking with analysts, we're customizgin what granularity of information is presented
name <- c('Clean Paper, Cardboard', "Metal, Glass, Plastic, Cartons", "Organics", "Other", "Textiles", "Diverted Materials")
percent <- c(17,17,34,23,6,3)

#sanity check
sum(percent)

waste_custom <- data.table(name, percent)

setorder(waste_custom, cols = percent)



barplot_colors <- c("#D05D4E","#F59F00","#228AE6","#12B886","#23417D","#A07952")
              #  ,"#82C91E","#CACACA","#2F56A6","#BE4BDB", "#B63F26")




ggplot(waste_custom, aes(x=reorder(waste_custom$name, waste_custom$percent), y=sort(waste_custom$percent, decreasing = TRUE),
           fill=as.factor(name))) + 
geom_bar(stat = "identity") +
coord_flip() +
scale_fill_manual(values=barplot_colors) +
xlab("Tonnage") + ylab("Material")+
geom_text(# Filter data first
  aes(label=paste0(percent, '%')), nudge_y = 5, size=3) +
theme_ipsum(axis_title_just = "mc",
            base_size = 8,
            axis_title_size = 11,
            axis_text_size = 10) +
theme(legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      text = element_text(family = "Open Sans"),
      plot.title = element_text(family = "Georgia",size = 14),
      axis.text.y = element_text(margin = margin(t = 0, r = -16, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
ggtitle("NYC Waste Composition 2017, Landfill Subset",) +
labs(caption = "Source: New York City Department of Sanitation")

ggsave("/bw_images/landfill_subset_categorizaiton.png", plot = p, path = getwd(), width = 8.5, height = 5, units = "in", dpi = 300)







ggdonutchart(waste, x="x2017",  label = "share", 
             fill = "material", color = "white", lab.adjust = 0, 
             lab.pos = "in", lab.font = c(size=3)) + 
  theme(legend.position="right",
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14)) +
  scale_fill_manual(values=donut_cols) +
  labs(title = "       New York City Waste Categorization, 2017 (Subset of Landfill Data)",
       subtitle = "",
       caption = "Source: New York City Department of Sanitation")



# DEP Green Infrastructure Map --------------------------------------------

gi_data <- read_sf("data/input/DEP Green Infrastructure/geo_export_3d6798f2-1f40-4bf8-84d1-a4c523865dd8.shp") %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84')

gi_pal <- colorFactor(palette = c("#228AE6","#3b8f03","#b81a1a","#db12d5"),
                      levels = unique(gi_data$sewer_type))
                      
leaflet(data = gi_data,
        options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(color = ~gi_pal(sewer_type),
                   radius = .5) %>% 
  addLegend(title = paste0("DEP Green Infrastructure Projects", "<br>", "Colored by Sewer Type"), pal = gi_pal, values = gi_data$sewer_type, position = "topleft")



