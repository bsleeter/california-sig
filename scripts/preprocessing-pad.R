library(raster)
library(sf)
library(fasterize)

crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +datum=NAD83"
eco = raster("data/initial-conditions/ic-ecoregion.tif")

all_on = reclassify(eco, c(0,Inf,1))
writeRaster(all_on, "data/spatial-multipliers/sm-fire-insect-reset.tif")

pad = st_read("I:/GIS-Vector/Protected Areas/PADUS2_0_Shapefiles/PADUS2_0Designation.shp")
pad = st_read("I:/GIS-Vector/Protected Areas/PADUS2_0_Shapefiles/PADUS2_0Easement.shp")
pad$GAP_Sts = as.numeric(pad$GAP_Sts)

pad.gap1 = pad %>% filter(GAP_Sts == 1)
pad.gap1.r = fasterize(pad.gap1, eco, field="GAP_Sts")
pad.gap1.r[is.na(pad.gap1.r)] = 5
pad.gap1.r = mask(pad.gap1.r,eco)

pad.gap2 = pad %>% filter(GAP_Sts == 2)
pad.gap2.r = fasterize(pad.gap2, eco, field="GAP_Sts")
pad.gap2.r[is.na(pad.gap2.r)] = 5
pad.gap2.r = mask(pad.gap2.r,eco)

pad.gap3 = pad %>% filter(GAP_Sts == 3)
pad.gap3.r = fasterize(pad.gap3, eco, field="GAP_Sts")
pad.gap3.r[is.na(pad.gap3.r)] = 5
pad.gap3.r = mask(pad.gap3.r,eco)

pad.gap4 = pad %>% filter(GAP_Sts == 4)
pad.gap4.r = fasterize(pad.gap4, eco, field="GAP_Sts")
pad.gap4.r[is.na(pad.gap4.r)] = 5
pad.gap4.r = mask(pad.gap4.r,eco)


pad.gap = stack(pad.gap1.r, pad.gap2.r, pad.gap3.r, pad.gap4.r)
pad.gap = min(pad.gap)

pad.eas = stack(pad.gap1.r, pad.gap2.r, pad.gap3.r, pad.gap4.r)
pad.eas = min(pad.eas)

pad_all = stack(pad.gap, pad.eas)
pad_all = min(pad_all)

plot(pad.gap)
plot(pad.eas)
plot(pad_all)

writeRaster(pad_all, "data/spatial-multipliers/pad20-gapstatus.tif", format="GTiff", overwrite=T, options="COMPRESS=DEFLATE", datatype="INT2S")




#fxn to execute a conditional statement
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)
}



# Urbanization Spatial Multiplier
census = st_read("I:/GIS-Vector/Boundary and Census/Urban Areas/cb_2015_us_ua10_500k/cb_2015_us_ua10_500k.shp")
census$LSAD10 = as.numeric(census$LSAD10)
census = st_transform(census, crs=crs(eco))

censusR = fasterize(census, eco, field = "LSAD10")
censusR = reclassify(censusR, c(-Inf,1.5,10, 1.6,2.5,2, 2.6,Inf,1))
censusR[is.na(censusR)] = 1
censusR = mask(censusR, eco)
plot(censusR)

urbMult = reclassify(pad_all, c(0,3.5,0, 3.6,Inf,1))
plot(urbMult)

urbMultFinal = Con(urbMult == 0, 0, censusR)
plot(urbMultFinal)

writeRaster(urbMultFinal, "data/spatial-multipliers/sm-urbanization.tif", format="GTiff", overwrite=TRUE, datatype="INT1U")



# Ag Expansion Spatial Multiplier
# Prohibits ag expanion on all Gap Status 1-3 lands
agExpMult = reclassify(pad_all, c(0,3.5,0, 3.6,Inf,1)) 
plot(agExpMult)
writeRaster(agExpMult, "data/spatial-multipliers/sm-ag-expansion.tif", format="GTiff", overwrite=TRUE, datatype="INT1U")


# AForest Harvest Spatial Multiplier
# Prohibits harvest on all Gap Status 1-2 lands
harvMult = reclassify(pad_all, c(0,2.5,0, 2.6,Inf,1)) 
plot(harvMult)
writeRaster(harvMult, "data/spatial-multipliers/sm-harvest.tif", format="GTiff", overwrite=TRUE, datatype="INT1U")








