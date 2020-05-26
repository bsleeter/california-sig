

library(raster)
library(sf)
library(tidyverse)




# Get a Landfire attribute table
dbf = list.files(paste0(dirs[1]), pattern = "*.dbf$", recursive = T)
rat = read.dbf(paste0(dirs[1], "/", dbf)[2]) %>% as_tibble()

# Get a list of the disturbance types
dist_type = unique(rat$Dist_Type)


# Fire types
fire_types_high = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "High")
fire_types_med = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Medium")
fire_types_low = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Low")




# Create a raster stack for each years Landfire disturbance output
files = paste0(dirs, "/", list.files(dirs, pattern = "*.tif$", recursive = T))[1:18]
rstack = stack(files)




##### High severity Fire #####
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
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(fire_high_zonal, "data/distributions/distribution-fire-high-severity.csv")







##### Medium severity Fire #####
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
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(fire_med_zonal, "data/distributions/distribution-fire-medium-severity.csv")







##### Low severity Fire #####
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
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(fire_low_zonal, "data/distributions/distribution-fire-low-severity.csv")
