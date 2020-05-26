
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






# Get a Landfire attribute table
dbf = list.files(paste0(dirs[1]), pattern = "*.dbf$", recursive = T)
rat = read.dbf(paste0(dirs[1], "/", dbf)[2]) %>% as_tibble()

# Get a list of the disturbance types
dist_type = unique(rat$Dist_Type)



# Insect/drought types
insect_types_high = rat %>% filter(Dist_Type %in% c("Insecticide", "Insects", "Disease", "Chemical", "Insects/Disease", "Herbicide", "Biological", "Weather"), Severity == "High")
insect_types_med = rat %>% filter(Dist_Type %in% c("Insecticide", "Insects", "Disease", "Chemical", "Insects/Disease", "Herbicide", "Biological", "Weather"), Severity == "Medium")
insect_types_low = rat %>% filter(Dist_Type %in% c("Insecticide", "Insects", "Disease", "Chemical", "Insects/Disease", "Herbicide", "Biological", "Weather"), Severity == "Low")



##### High severity Insect/Drought #####
outdir = "data/spatial-multipliers/insects-high-severity/"
type = "insects-high-severity"
rc = insect_types_high %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
insect_high = reclassify(rstack, rcl = rc)
insect_high = reclassify(insect_high, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(insect_high, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

# Zonal summary by Ecoregion - Create historical distribution datasheet
insect_high_zonal = as_tibble(zonal(insect_high, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Insects: High Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Insects: High Severity",
         ExternalVariableTypeID = "Insects",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Timestep and iteration",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(insect_high_zonal, "data/distributions/distribution-finsects-high-severity.csv")

ggplot(insect_high_zonal, aes(x=ExternalVariableMin, y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~PrimaryStratumID)












