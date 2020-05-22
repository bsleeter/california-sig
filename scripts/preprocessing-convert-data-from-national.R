


library(tidyverse)
library(raster)
library(sf)
library(doParallel)
library(foreach)

ca_counties = raster("E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_Counties_1km.tif")
plot(ca_counties)
writeRaster(ca_counties, "data/initial-conditions/ic-counties.tif")

us_stateclass = raster("F:/national-assessment/data/initial-conditions/final/ic-state-class.tif")
us_ecoregions = raster("F:/national-assessment/data/initial-conditions/final/ic-ecoregion.tif")
us_age = raster("F:/national-assessment/data/initial-conditions/final/ic-age.tif")
us_land = raster("F:/national-assessment/data/initial-conditions/final/ic-land-managers.tif")

ca_stateclass = projectRaster(us_stateclass, ca_counties, method="ngb")
ca_stateclass = mask(ca_stateclass, ca_counties)
plot(ca_stateclass)
writeRaster(ca_stateclass, "data/initial-conditions/ic-state-class.tif")

ca_ecoregions = projectRaster(us_ecoregions, ca_counties, method="ngb")
ca_ecoregions = mask(ca_ecoregions, ca_counties) 
plot(ca_ecoregions)
writeRaster(ca_ecoregions, "data/initial-conditions/ic-ecoregion.tif")

ca_age = projectRaster(us_age, ca_counties, method="ngb")
ca_age = mask(ca_age, ca_counties) 
plot(ca_age)
writeRaster(ca_age, "data/initial-conditions/ic-age.tif")

ca_land = projectRaster(us_land, ca_counties, method="ngb")
ca_land = mask(ca_land, ca_counties) 
plot(ca_land)
writeRaster(ca_land, "data/initial-conditions/ic-land-managers.tif")


##### Subset Initial stocks to California ##### 
list_stocks = list.files("F:/national-assessment/data/initial-stocks/mapped")[c(7:11,14:17,19:21,24:25)]
us_stocks = stack(paste0("F:/national-assessment/data/initial-stocks/mapped/", list_stocks))
ca_stocks = projectRaster(us_stocks, ca_counties)
ca_stocks = mask(ca_stocks, ca_counties)
plot(ca_stocks)
writeRaster(ca_stocks, format="GTiff", overwrite=T, bylayer=T, filename=paste0("data/initial-stocks/", list_stocks))




##### Subset spatial flow multipliers to California ##### 
dir = getwd()
gcmList = list.files("F:/national-assessment/data/flow-spatial-multipliers/q10Slow")
rcpList = c("rcp45", "rcp85")

indir = "F:/national-assessment/data/flow-spatial-multipliers/q10Slow/"
outdir = "data/flow-spatial-multipliers/q10Slow/"

cl = makeCluster(21)
registerDoParallel(cl)
startTime = Sys.time()

rcp = "rcp85" # Run code for each RCP
var = "q10Slow"
foreach(i = gcmList, .packages="raster") %dopar% {
  #i = "bcc-csm1-1"
  r = stack(paste(indir, i, "/", rcp, "/", i, ".", rcp, ".", var, "_", seq(2002,2099,1), ".tif", sep=""))
  r = projectRaster(r, ca_counties)
  r = mask(r, ca_counties)
  writeRaster(r, filename=paste(outdir, i, "/", rcp, "/", names(r), sep=""), format = "GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT2S")
}

stopCluster(cl)

endTime = Sys.time()
time = endTime-startTime

# Historical Growth data
list_hist = list.files("F:/national-assessment/data/flow-spatial-multipliers/growth/historical")
hist_growth = stack(paste0("F:/national-assessment/data/flow-spatial-multipliers/growth/historical/", list_hist))
hist_growth = projectRaster(hist_growth, ca_counties)
hist_growth = mask(hist_growth, ca_counties)
plot(hist_growth)
writeRaster(hist_growth, format="GTiff", overwrite=T, bylayer=T, filename=paste0("data/flow-spatial-multipliers/growth/historical/", list_hist), options="COMPRESS=DEFLATE", datatype="INT2S")

# Historical Q10 Fast data
list_hist = list.files("F:/national-assessment/data/flow-spatial-multipliers/q10Fast/historical")
hist_q10Fast = stack(paste0("F:/national-assessment/data/flow-spatial-multipliers/q10Fast/historical/", list_hist))
hist_q10Fast = projectRaster(hist_q10Fast, ca_counties)
hist_q10Fast = mask(hist_q10Fast, ca_counties)
plot(hist_q10Fast)
writeRaster(hist_q10Fast, format="GTiff", overwrite=T, bylayer=T, filename=paste0("data/flow-spatial-multipliers/q10Fast/historical/", list_hist), options="COMPRESS=DEFLATE", datatype="INT2S")

# Historical Q10 Slow data
list_hist = list.files("F:/national-assessment/data/flow-spatial-multipliers/q10Slow/historical")
hist_q10Slow = stack(paste0("F:/national-assessment/data/flow-spatial-multipliers/q10Slow/historical/", list_hist))
hist_q10Slow = projectRaster(hist_q10Slow, ca_counties)
hist_q10Slow = mask(hist_q10Slow, ca_counties)
plot(hist_q10Slow)
writeRaster(hist_q10Slow, format="GTiff", overwrite=T, bylayer=T, filename=paste0("data/flow-spatial-multipliers/q10Slow/historical/", list_hist), options="COMPRESS=DEFLATE", datatype="INT2S")




##### Filter State Attributes to California State Classes ##### 
list_stateclass = unique(ca_stateclass)
list_stateclass_names = read_csv("F:/national-assessment/data/definitions/state-class-types.csv") %>%
  filter(ID %in% list_stateclass) %>%
  mutate(StateClassID = paste0(StateLabelXID, ": ", StateLabelYID))
unique(list_stateclass_names$StateClassID)


ca_state_attributes = read_csv("F:/national-assessment/data/state-attributes/state-attribute-values-harvest.csv") %>%
  filter(StateClassID %in% list_stateclass_names$StateClassID) %>%
  write_csv("data/state-attributes/state-attribute-values-harvest.csv")


### Filter flow pathways to California state classes ###
flow_pathways = read_csv("F:/national-assessment/data/stock-flow-model/flow-pathways.csv") %>%
  filter(FromStateClassID %in% list_stateclass_names$StateClassID) %>%
  write_csv("data/stock-flow-model/flow-pathways.csv")





##### Spatial Multipliers #####

urbMult = raster("F:/national-assessment/data/spatial-multipliers/projection-urbanization.tif")
urbMult = projectRaster(urbMult, ca_counties)
urbMult = mask(urbMult, ca_counties)
plot(urbMult)
writeRaster(urbMult, format="GTiff", overwrite=T, filename=paste0("data/spatial-multipliers/sm-urbanization.tif"), options="COMPRESS=DEFLATE", datatype="INT2S")
