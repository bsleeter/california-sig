

# Header ------------------------------------------------------------------
# Code to calculate annual disturbance rates due to insect mortality using US Forest Service Aerial detection Survey data
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions and Spatial Multipliers of insect mortality formatted for use within the LUCAS model
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28




# Setup -------------------------------------------------------------------

# Load libraries
library(raster)
library(sf)
library(tidyverse)

# Read in California Ecoregions Raster
ecoregions = raster("data/initial-conditions/ic-ecoregion.tif")

# Read in California Ecoregions Table
ecoregion_df = read_csv("data/definitions/ecoregions.csv")

# Set Severity breakpoints
b1 = 3 # Top break point (trees/acre) for low severity
b2 = 10 # Top break point (trees/acre) for medium severity



# Read Raw ADS Data -------------------------------------------------------

# Read in raw ADS data by Region (downloaded to local disk)
# ADS data available from: https://www.fs.fed.us/foresthealth/applied-sciences/mapping-reporting/detection-surveys.shtml

ads = st_read("I:/GIS-Vector/Aerial Detection Surveys/20200526/CA_Region5_AllYears.gdb/CONUS_Region5_AllYears.gdb", layer="DAMAGE_AREAS_FLAT_AllYears_CONUS_Rgn5") %>%
  dplyr::select(DAMAGE_AREA_ID,DAMAGE_TYPE,SURVEY_YEAR,LEGACY_TPA,LEGACY_FOREST_TYPE,SHAPE)
ads = ads %>% 
  filter(SURVEY_YEAR<=2019, SURVEY_YEAR>=1997, DAMAGE_TYPE=="Mortality", LEGACY_TPA>0) %>%
  mutate(SEVERITY = ifelse(LEGACY_TPA<=b1,1, ifelse(LEGACY_TPA>b1 & LEGACY_TPA<=b2, 2, 3))) %>% arrange(SURVEY_YEAR)

# Convert data to multipolygon and convert to raster based on severity (value) and survey year (band)
adsCast = st_cast(ads, "MULTIPOLYGON") 
adsStack = fasterize(adsCast, ecoregions, field="SEVERITY", by="SURVEY_YEAR")
adsStack[is.na(adsStack)] = 0

# Project and mask data to California projection, extent, and resolution
adsStack = projectRaster(adsStack, ecoregions, method="ngb")
adsStack = mask(adsStack, ecoregions)
plot(adsStack$X2015)

# Create a raster stack for each severity class
adsLow = reclassify(adsStack, c(0,1.5,1, 1.6,Inf,0))
adsMed = reclassify(adsStack, c(0,1.5,0, 1.6,2.5,1, 2.6,Inf,0))
adsHigh = reclassify(adsStack, c(0,2.5,0, 2.6,3.5,1, 3.6,Inf,0))
plot(adsHigh$X2015)
names(adsHigh)




# Write Spatial Multipliers -----------------------------------------------

outdir = "data/spatial-multipliers/insects-high-severity/"
outfile = "insects-high-severity-"
writeRaster(adsHigh, paste0(outdir, outfile, seq(1998,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

outdir = "data/spatial-multipliers/insects-medium-severity/"
outfile = "insects-medium-severity-"
writeRaster(adsMed, paste0(outdir, outfile, seq(1998,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")

outdir = "data/spatial-multipliers/insects-low-severity/"
outfile = "insects-low-severity-"
writeRaster(adsLow, paste0(outdir, outfile, seq(1998,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")




# Write Historical Distributions ------------------------------------------

# Zonal summary by Ecoregion for high severity
ads_high_zonal = as_tibble(zonal(adsHigh, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: High Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Insect: High Severity",
         ExternalVariableTypeID = "Insect",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(ads_high_zonal, "data/distributions/distribution-insect-high-severity.csv")


# Zonal summary by Ecoregion for medium severity
ads_medium_zonal = as_tibble(zonal(adsMed, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: Medium Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Insect: Medium Severity",
         ExternalVariableTypeID = "Insect",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(ads_medium_zonal, "data/distributions/distribution-insect-medium-severity.csv")


# Zonal summary by Ecoregion for low severity
ads_low_zonal = as_tibble(zonal(adsLow, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: Low Severity [Type]") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  mutate(DistributionTypeID = "Insect: Low Severity",
         ExternalVariableTypeID = "Insect",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Iteration and Timestep",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(StratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(ads_low_zonal, "data/distributions/distribution-insect-low-severity.csv")


# Merge high, medium, and low severity data frames together and plot to check for anomolies
df = bind_rows(ads_high_zonal, ads_medium_zonal, ads_low_zonal)

ggplot(df, aes(x=ExternalVariableMin, y=Value, color=DistributionTypeID)) +
  geom_line() +
  geom_point() +
  facet_wrap(~PrimaryStratumID)
