
library(foreign)
library(raster)
library(sf)
library(tidyverse)

# Read in ICLUS SSP projections
zonal_ssp = read_csv("docs/ssp/iclus-ssp-zonal-county-summary.csv")

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")

# Read in California Ecoregions raster
ecoregions = raster("data/initial-conditions/ic-ecoregion.tif")
ecoregion_df = read_csv("data/definitions/ecoregions.csv")


# Each Landfire disturbance year contains two tiles for California
# This code merges the two tiles and creates a single tiff output file in year directory

years = seq(1999,2016)
dirs = paste0("I:/GIS-Raster/Landfire/California/disturbance/", years)

# Start a parallel cluster
cl = makeCluster(18)
registerDoParallel(cl)
foreach(i = dirs, .packages = c("raster", "stringr")) %dopar% {
  #i = dirs[1]
  dir = i
  d1 = list.dirs(dir)[3]
  d2 = list.dirs(dir)[5]
  
  l1 = list.files(d1, pattern = "*.tif$")
  l2 = list.files(d2, pattern = "*.tif$")
  
  r1 = raster(paste0(d1,"/",l1))
  r1 = projectRaster(r1, counties, method="ngb")
  
  r2 = raster(paste0(d2,"/",l2))
  r2 = projectRaster(r2, counties, method="ngb")
  
  r = merge(r1,r2)
  r = mask(r, counties)
  writeRaster(r, paste0(i, "/", "landfire-disturbance-", str_sub(i, start = -4), ".tif"), format="GTiff")
}
stopCluster(cl)
















