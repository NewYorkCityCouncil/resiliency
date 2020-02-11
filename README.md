# resiliency
for resiliency sustainability group - looking at heat and flooding areas

We are focusing on the summer months from 2014 to 2019 in NYC.

## Data Structure

- data
  - input
    - Parks Properties (https://data.cityofnewyork.us/City-Government/Parks-Properties/k2ya-ucmv)
    - Airport Polygon (https://data.cityofnewyork.us/City-Government/Airport-Polygon/xfhz-rhsk)
    - landsat_st (from Landsat folder on G drive)
    - Ground_Monitor_Temps_NYC (from Landsat folder on G drive)

## Getting Data

- US Landsat 4-8 ARD: [Provisional Surface Temperature (ST)](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-landsat-archives-landsat-level-2-provisional-surface?qt-science_center_objects=0#qt-science_center_objects)
  1. Make an account at (https://earthexplorer.usgs.gov/)
  2. Install [Bulk Download Application](https://earthexplorer.usgs.gov/bulk)
  3. On Earth Exloper site search panel, select desired criteria:
      - Date Range: 2014 to 2020
      - Datasets: US Landsat 4-8 ARD
      - Tile grid horizontal: 29 (NYC)
      - Tile grid vertical: 7 (NYC)
        * search for tile grid [here](https://www.usgs.gov/media/images/conterminous-us-landsat-analysis-ready-data-ard-tiles)
  4. Follow [BIG DATA Download](https://blogs.fu-berlin.de/reseda/landsat-big-data-download/#3) instructions from the blog site (blogs.fu-berlin.de) 
     - Where the instructions say "Choose “Non-Limited Results” and “CSV” in order to export the metadata of every single file found to a csv-file (which is a text file)" choose "Comma (,) Delimited" format instead.
     
- Ground Monitor Temperature:
  1. Select your local stations. (Central Park, LaGuardia, Kennedy)
  [Local Climatological Data (LCD)](https://www.ncdc.noaa.gov/cdo-web/datatools/lcd)
  2. You need to add the data to your cart, then go to your cart, where you can select that you want a csv and subset to the dates you are interested in.
     - [Central Park](https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:94728/detail)

## Validating Data
- Satellite Readings
  1. Cloud Cover: https://landsat.usgs.gov/landsat-8-cloud-cover-assessment-validation-data#Urban
  2. Comparing Ground Monitor temperatures to Satellite readings
