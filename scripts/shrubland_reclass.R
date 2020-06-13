## california-sig
## Reclassify shrublands 
## in State Class raster and
## State Class types table
## Paul C. Selmants
## 2020-06-12

# load required R packages
library(raster)
library(dplyr)

# read in State Class raster
sc <- raster("./data/initial-conditions/ic-state-class.tif")

# read in state class type table
sctable <- read.csv("./data/definitions/state-class-types.csv", 
	stringsAsFactors = FALSE)

# reclassification matrix for state class raster
shr_rcl <- c(50,51,52,51,53,51,55,51,56,51,57,51,60,51,62,51,63,51) %>%
	matrix(., ncol = 2, byrow = TRUE)

# reclassify state class raster into three shrubland classes
sc_shrub <- reclassify(sc, shr_rcl)

# write reclassified raster to GeoTIFF file
writeRaster(sc_shrub, "./data/initial-conditions/reclass-ic-state-class.tif")

#crosswalk table for shrub state classes
sc_shrub <- sctable %>%
	filter(StateLabelXID == "Shrubland") %>%
	filter(!StateLabelYID %in% c("Succulent", "Sand Shrubland", "Pocosin", 
		"Pinyon Juniper Shrubland")) %>%
	mutate(NewID = c(51, NA, rep(51,5), NA, 51, NA, rep(51,2), NA),
		NewLabelYID = c("Desert Scrub", NA, rep("Desert Scrub", 5), 
			NA, "Desert Scrub", NA, rep("Desert Scrub", 2), NA)) %>%
	select(Name:StateLabelYID, NewLabelYID, ID, NewID, Legend:IsAutoName) 

# add new columns to non-shrub classes
sc_NoShrub <- sctable %>%
	filter(!StateLabelXID == "Shrubland") %>%
	mutate(NewID = NA, 
		NewLabelYID = NA) %>%
	select(Name:StateLabelYID, NewLabelYID, ID, NewID, Legend:IsAutoName) 

# combine new shrub and non-shrub tables into single crosswalk table
sc_cw <- bind_rows(sc_NoShrub, sc_shrub) %>%
	arrange(ID)

# write reclassified table to .csv file
write.csv(sc_cw, "./data/definitions/reclass-state-class-types.csv") 

	


			