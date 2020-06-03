
# Header ------------------------------------------------------------------
# Code to calculate annual fire disturbance rates from the Landfire Disturbance
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions of fire disturbance.
# Landfire Disturbance data can be downloaded here: https://www.landfire.gov/disturbance_2.php
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)
library(foreign)


# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")

# Read in California Ecoregions raster
ecoregions = raster("data/initial-conditions/ic-ecoregion.tif")
ecoregion_df = read_csv("data/definitions/ecoregions.csv")

# Years of data
years = seq(1999,2016)

# Directory to where annual Landfire Disturbance data are stored
# Note: Data can be compiled from national mosaics or by downloading tiles. Data for this study were developed from tiles. 
# To create the mosaics used below run the preprocessing-landfire-disturbance script
# source(preprocessing-landfire-disturbance.R)
dirs = paste0("I:/GIS-Raster/Landfire/California/disturbance/", years)


# Get a Landfire attribute table
dbf = list.files(paste0(dirs[1]), pattern = "*.dbf$", recursive = T)
rat = read.dbf(paste0(dirs[1], "/", dbf)[2]) %>% as_tibble()

# Get a list of the unique disturbance types
dist_type = unique(rat$Dist_Type)






# Define Fire Types to be included and create raster stack----------------------------------------

# Fire types
fire_types_high = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "High")
fire_types_med = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Medium")
fire_types_low = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Low")


# Create a raster stack from each years Landfire disturbance output
files = paste0(dirs, "/", list.files(dirs, pattern = "*.tif$", recursive = T))[1:18]
rstack = stack(files)





# High Severity Fire ----------------------------------------------

# Create stack and reclassify rasters to include high severity fire only
outdir = "data/spatial-multipliers/fire-high-severity/"
type = "fire-high-severity"
rc = fire_types_high %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_high = reclassify(rstack, rcl = rc)
fire_high = reclassify(fire_high, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(fire_high, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

# Zonal summary by Ecoregion - Create historical distribution datasheet
fire_high_zonal = as_tibble(zonal(fire_high, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: High Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Fire: High Severity",
         ExternalVariableTypeID = "Fire",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  mutate(ValueDistributionTypeID = ifelse(Value==0, NA, "Normal")) %>%
  mutate(ValueDistributionSD = ifelse(Value==0, NA, ValueDistributionSD)) %>%
  mutate(ValueDistributionSD = ifelse(ValueDistributionSD==0, 1, ValueDistributionSD))
write_csv(fire_high_zonal, "data/distributions/distribution-fire-high-severity.csv")






# Medium Severity Fire ----------------------------------------------------

# Create stack and reclassify rasters to include medium severity fire only
outdir = "data/spatial-multipliers/fire-medium-severity/"
type = "fire-medium-severity"
rc = fire_types_med %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_med = reclassify(rstack, rcl = rc)
fire_med = reclassify(fire_med, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(fire_med, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

# Zonal summary by Ecoregion - Create historical distribution datasheet
fire_med_zonal = as_tibble(zonal(fire_med, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: Medium Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Fire: Medium Severity",
         ExternalVariableTypeID = "Fire",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  mutate(ValueDistributionTypeID = ifelse(Value==0, NA, "Normal")) %>%
  mutate(ValueDistributionSD = ifelse(Value==0, NA, ValueDistributionSD)) %>%
  mutate(ValueDistributionSD = ifelse(ValueDistributionSD==0, 1, ValueDistributionSD))
write_csv(fire_med_zonal, "data/distributions/distribution-fire-medium-severity.csv")







# Low Severity Fire ----------------------------------------------------

# Create stack and reclassify rasters to include low severity fire only
outdir = "data/spatial-multipliers/fire-low-severity/"
type = "fire-low-severity"
rc = fire_types_low %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_low = reclassify(rstack, rcl = rc)
fire_low = reclassify(fire_low, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(fire_low, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

# Zonal summary by Ecoregion - Create historical distribution datasheet
fire_low_zonal = as_tibble(zonal(fire_low, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: Low Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Fire: Low Severity",
         ExternalVariableTypeID = "Fire",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>%
  mutate(ValueDistributionTypeID = ifelse(Value==0, NA, "Normal")) %>%
  mutate(ValueDistributionSD = ifelse(Value==0, NA, ValueDistributionSD)) %>%
  mutate(ValueDistributionSD = ifelse(ValueDistributionSD==0, 1, ValueDistributionSD))
write_csv(fire_low_zonal, "data/distributions/distribution-fire-low-severity.csv")
