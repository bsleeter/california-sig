





library(raster)
library(tidyverse)

counties = raster("data/initial-conditions/ic-counties.tif")
counties_tbl = read_csv("data/definitions/counties-reclass-multiprocessing-regions.csv") %>%
  select(from=ID, becomes=NewID) %>%
  mutate(to = from) %>%
  select(from, to, becomes) %>%
  mutate(from = from-0.4, to = to+0.5)
rcl = as.matrix(counties_tbl)


mpRaster = reclassify(counties, rcl)
plot(mpRaster)
hist(mpRaster)
writeRaster(mpRaster, "data/initial-conditions/multi-processing-raster.tif", format="GTiff", overwrite=T)
