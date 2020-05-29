


# Header ------------------------------------------------------------------
# Code to calculate process transition rates from the National Land Cover Database
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces a table of transition rates between NLCD LULC classes
# NLCD data can be downloaded here: https://www.mrlc.gov/data
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)



# Read and Project NLCD Data ----------------------------------------------

# NLCD 2001
nlcd01 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2001_Land_Cover_L48_20190424.img")
nlcd01 = projectRaster(nlcd01, counties, method = "ngb")
nlcd01 = mask(nlcd01, counties)

# NLCD 2006
nlcd06 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2006_Land_Cover_L48_20190424.img")
nlcd06 = projectRaster(nlcd06, counties, method = "ngb")
nlcd06 = mask(nlcd06, counties)

# NLCD 2011
nlcd11 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2011_Land_Cover_L48_20190424.img")
nlcd11 = projectRaster(nlcd11, counties, method = "ngb")
nlcd11 = mask(nlcd11, counties)

# NLCD 2016
nlcd16 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2016_Land_Cover_L48_20190424.img")
nlcd16 = projectRaster(nlcd16, counties, method = "ngb")
nlcd16 = mask(nlcd16, counties)

nlcd = stack(nlcd01, nlcd06, nlcd11, nlcd16)

v = tibble(
  lc01 = values(nlcd01),
  lc06 = values(nlcd06),
  lc11 = values(nlcd11),
  lc16 = values(nlcd16))

write_csv(v, "docs/nlcd/nlcd-change-dataframe.csv")