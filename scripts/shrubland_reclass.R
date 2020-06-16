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
writeRaster(sc_shrub, "./data/initial-conditions/ic-state-class-3shrub.tif")

#crosswalk table for shrub state classes
sc_shrub <- sctable %>%
	filter(StateLabelXID == "Shrubland") %>%
	filter(!StateLabelYID %in% c("Succulent", "Sand Shrubland", "Pocosin", 
		"Pinyon Juniper Shrubland")) %>%
	mutate(NewID = c(51, NA, rep(51,5), NA, 51, NA, rep(51,2), NA),
		NewLabelYID = c("Desert Scrub", NA, rep("Desert Scrub", 5), 
			NA, "Desert Scrub", NA, rep("Desert Scrub", 2), NA)) %>%
	dplyr::select(Name:StateLabelYID, NewLabelYID, ID, NewID, Legend:IsAutoName) 

# Create the new SyncroSim datasheet for Shrubland classes
sc_shrub_new = data.frame(Name = c("Shrubland: Desert Scrub", "Shrubland: Pacific Coastal Shrub", "Shrubland: Chaparral", "Shrubland: Post Fire Shrub"),
                          StateLabelXID = "Shrubland",
                          StateLabelYID = c("Desert Scrub", "Pacific Coastal Shrub", "Chaparral", "Post Fire Shrub"),
                          ID = c(51,58,61,1000),
                          Legend = NA, 
                          Description = NA,
                          IsAutoName = NA)

# add new columns to non-shrub classes
sc_NoShrub <- sctable %>%
	filter(!StateLabelXID == "Shrubland") %>%
	mutate(NewID = NA, 
		NewLabelYID = NA) %>%
  dplyr::select(Name:StateLabelYID, NewLabelYID, ID, NewID, Legend:IsAutoName) 

# combine new shrub and non-shrub tables into single crosswalk table
sc_cw <- bind_rows(sc_NoShrub, sc_shrub_new) %>%
	arrange(ID) %>%
  dplyr::select(-NewLabelYID, -NewID)

# write reclassified table to .csv file
write_csv(sc_cw, "./data/definitions/state-class-types.csv") 

	
# Create StateLabelXID file
slxid = data.frame(Name = unique(sc_cw$StateLabelXID))
write_csv(slxid, "data/definitions/state-label-xid.csv")

# Create StateLabelYID file
slyid = data.frame(Name = unique(sc_cw$StateLabelYID))
write_csv(slyid, "data/definitions/state-label-yid.csv")

# Create StateLabelYID file
sct = data.frame(Name = unique(sc_cw$StateLabelYID))
write_csv(slyid, "data/definitions/state-label-yid.csv")

			