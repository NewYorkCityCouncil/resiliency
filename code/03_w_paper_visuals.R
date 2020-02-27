library(tidyverse)
library(sf)
library(leaflet)
library(pdftools)
library(ggplot2)
library(ggpubr)
library(RSocrata)
library(hrbrthemes)



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


flood_points_sf <- st_as_sf(flood_points) %>% 
  filter(flooding_normalized > 0)


# Use kernel density estimate (kde) to create heatmap of city; using higher
# row/column values for a resolution that better fits the scale of the data.
kde_flood <- sp.kde(x = flood_points, y = flood_points$street_flooding_complaints,  
                   nr = 600, nc = 600, mask = TRUE,  bw = .018)
plot(kde_flood)


# crop this new raster to nyc
nyc2 <- st_transform(nyc, projection(kde_flood))


#crop & mask the raster files to poylgon extent/boundary
kde_flood_masked <- mask(kde_flood, nyc2)
kde_flood_crop <- crop(kde_flood_masked, nyc2)


flood_pal <- colorNumeric(colorRamps::matlab.like(15), values(kde_flood_crop),
                         na.color = "transparent")

# flood_points_pal <- colorNumeric(colorRamps::matlab.like(10), domain = flood_points_sf$flooding_normalized)
# 
# flood_points_popup <- as.character(flood_points_sf$flooding_normalized)


sequence <- seq(min(flood_points$street_flooding_complaints), max(flood_points$street_flooding_complaints), 5)
flood_legend_pal <- colorBin(palette = colorRamps::matlab.like(15), bins = sequence, domain = sequence)


leaflet(flood_points_sf,
        options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addRasterImage(kde_flood_crop, colors = flood_pal, opacity = 0.4) %>% 
  addLegend(title = "Number of Complaints", 
            pal = flood_legend_pal, 
            values = sequence,
            position = "topleft")


options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = ~curbside_pal(curbside_pickup_status), popup = curbside_pop,
              weight = .8) %>% 
  addLegend(pal = curbside_pal, values = curbside_pickup_shape$curbside_pickup_status,
            position = "topleft")

# %>% 
#   addCircleMarkers(radius = 1, color = ~flood_points_pal(flooding_normalized), popup = flood_points_popup)


# Waste Characterization Analysis ------------------------------------------





trash_pdf <- pdf_text("data/input/2017 NYC Waste Characterization Study p16.pdf") %>% 
  readr::read_lines()

waste_raw <- trash_pdf[3:19] %>% 
  trimws(which = "both") %>% 
  strsplit(split = "\\s{2,}")

waste_variables <- waste_raw[1] %>% 
  unlist()

waste_variables <- c("Material", waste_variables)

waste_data_lines <- waste_raw[c(3:4, 6:8, 10:13, 15:17)]

waste_categories <- waste_raw[c(2,5,9,14)] %>% 
  unlist() 

waste_categories <- waste_categories[c(1,5,9,13)]

# number of items for each category: 2, 3, 4, 2

category <- c(rep(waste_categories[1], 2),
              rep(waste_categories[2], 3),
              rep(waste_categories[3], 4),
              rep(waste_categories[4], 2))

waste_df <- plyr::ldply(waste_data_lines)
colnames(waste_df) <- waste_variables

waste <- as_tibble(waste_df) %>% 
  janitor::clean_names() %>% 
  select(material, x2017) %>% 
  mutate(x2017 = as.numeric(gsub("%", "", x2017))) %>% 
  filter(!is.na(x2017)) %>% 
  mutate(category = category,
         share = paste0(x2017, "%"))



# donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1)) {
#   group <- rep_len(group, length(x))
#   ug  <- unique(group)
#   tbl <- table(group)[order(ug)]
#   
#   col <- if (is.null(col))
#     seq_along(ug) else rep_len(col, length(ug))
#   col.main <- Map(rep, col[seq_along(tbl)], tbl)
#   col.sub  <- lapply(col.main, function(x) {
#     al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
#     Vectorize(adjustcolor)(x, alpha.f = al)
#   })
#   
#   plot.new()
#   
#   par(new = TRUE)
#   pie(x, border = NA, radius = radius[2L],
#       col = unlist(col.sub), labels = labels)
#   
#   par(new = TRUE)
#   pie(x, border = NA, radius = radius[1L],
#       col = unlist(col.main), labels = NA)
# }
# 
# 
# par(mfrow = c(3,1), mar = c(4,4,0,4))
# with(waste,
#      donuts(share, category, sprintf('%s: %s%%', material, share),
#             col = c('cyan2','red','orange','green','dodgerblue2'))
# )


  
amount <- c(3, 2, 5, 25, 65)
Source <- c("Other", "Renewables", "Hydropower", "Nuclear", "Fossil Fuels")
data <- data.table::data.table(amount, Source)
data[, label := paste(amount, "%", sep="")]

library(data.table)
waste_data_table <- as.data.table(waste)

ggdonutchart(waste, x="x2017",  label = "share", 
             fill = "material", color = "white", lab.adjust = 0, 
             lab.pos = "out", lab.font = c(size=3), 
             palette = "viridis") + theme(legend.position="right",
                                          panel.grid.major.y = element_blank(), 
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          panel.grid.minor.y = element_blank(),
                                          #text = element_text(family = "Open Sans"),
                                          plot.title = element_text(family = "Georgia",size = 14)) +
  labs(title = "       New York City Energy Consumption, 2017",
       subtitle = "",
       caption = "Source: Mayor's Office of Sustainability")




amount <- waste$x2017
Source <- waste$material
data <- data.table(amount, Source)
data[, label := paste(amount, "%", sep="")]

ggdonutchart(data, x="amount",  label = "label", 
             fill = "Source", color = "white", lab.adjust = 0, 
             lab.pos = "out", lab.font = c(size=3), 
             palette = "viridis") + theme(legend.position="right",
                                          panel.grid.major.y = element_blank(), 
                                          panel.grid.major.x = element_blank(),
                                          panel.grid.minor.x = element_blank(),
                                          panel.grid.minor.y = element_blank(),
                                          #text = element_text(family = "Open Sans"),
                                          plot.title = element_text(family = "Georgia",size = 14)) +
  labs(title = "       New York City Energy Consumption, 2017",
       subtitle = "",
       caption = "Source: Mayor's Office of Sustainability")




waste_raw <- read_csv("https://data.cityofnewyork.us/resource/k3ks-jzek.csv")







https://data.cityofnewyork.us/resource/k3ks-jzek.csv