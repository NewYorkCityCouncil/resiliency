


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



library(stars)
library(raster)
library(sf)
library(velox)

"data/input/landsat_st/LC08_CU_029007_20190830_20190919_C01_V01_ST.tif"
"data/input/landsat_st/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif"
"data/input/landsat_st/LC08_CU_029007_20180710_20190614_C01_V01_ST.tif"


## Define the function
gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')}
                        if (!file.exists(pypath)) stop("Can\'t find gdal_polygonize.py on your system.")
                        owd <- getwd()
                        on.exit(setwd(owd))
                        setwd(dirname(pypath))
                        if (!is.null(outshape)) {
                        outshape <- sub('\\.shp$', '', outshape)
                        f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
                        if (any(f.exists))
                        stop(sprintf('File already exists: %s',
                        toString(paste(outshape, c('shp', 'shx', 'dbf'),
                        sep='.')[f.exists])), call.=FALSE)
                        } else outshape <- tempfile()
                        if (is(x, 'Raster')) {
                        require(raster)
                        writeRaster(x, {f <- tempfile(fileext='.tif')})
                        rastpath <- normalizePath(f)
                        } else if (is.character(x)) {
                        rastpath <- normalizePath(x)
                        } else stop('x must be a file path (character string), or a Raster object.')
                        system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                        pypath, rastpath, gdalformat, outshape)))
                        if (isTRUE(readpoly)) {
                        shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
                        return(shp)
                        }
                        return(NULL)
  }


library(rasterVis)
r <- raster("data/input/landsat_st/LC08_CU_029007_20190830_20190919_C01_V01_ST.tif")
levelplot(r, margin=FALSE, col.regions=rainbow)
p <- gdal_polygonizeR(r)


x <- rasterToPoints(r, fun=NULL, spatial=TRUE)


tif = system.file("data/input/landsat_st/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif, package = "stars")
x = read_stars(tif)


filenames <- list.files("data/input/landsat_final_used_values", pattern="*.tif", full.names=TRUE)
ldf_r <- lapply(filenames, raster)

# aug3019 <- read_stars("data/input/landsat_st/LC08_CU_029007_20190922_20191001_C01_V01_ST.tif")
# crs(aug3019)
# plot(aug3019)
# augsf <- st_as_sf(aug3019)
# spdf_2 <- as(aug3019,'SpatialPointsDataFrame')


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
test_filenames = filenames[1:3]
ldf <- lapply(filenames, raster)
res <- lapply(ldf, temp_func_2)

#pull out just the date for the names of each dataframe
dataset_names <- str_extract(str_extract(filenames, pattern ="029007_[0-9]*"), pattern = "_[0-9]*")

# name the dataframes in the list
names(res) <- dataset_names

#join listed dataframes into single dataframe
raster_shape <- bind_rows(res, .id = "column_label") %>% 
  mutate(date = lubridate::ymd(str_replace(raster_shape$column_label, "_", ""))) %>% 
  filter(!is.na(max_temp))

# a lot of values are coming up as n/a - every third value, in fact.

# After looking at the raster images themselves, I believe it's because, while
# they are categorized under NYC, the satelite doesn't actually pass over much 
# of the city, if any. But they still count it as a NYC file. These will be 
# removed from the analysis
# 
# 
raster_test <- raster("data/input/landsat_st/LC08_CU_029007_20140908_20190503_C01_V01_ST.tif")
raster_test <- raster("data/input/landsat_st/LC08_CU_029007_20140604_20190504_C01_V01_ST.tif")
plot(raster_test)
rstr <- velox(raster_test)
sf <- st_transform(shapes, crs(raster_test))
temps <- rstr$extract(sp=sf$geometry)
sf$mean <- sapply(temps, mean, na.rm = TRUE)
sf$mean_temp <- k_to_f(sf$mean/10)
sf$max_temp <- sapply(temps, max)
sf$max_f <- k_to_f(sf$max_temp/10)
