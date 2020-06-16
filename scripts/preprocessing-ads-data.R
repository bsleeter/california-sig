

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

# Average Insect Probability by Ecoregion (Base Insect Probability) ----------------------------------------------------------

# Create stack and reclassify rasters to include all Insects
ads_all = reclassify(adsStack, c(0.5,Inf,1))


# Ecoregion Mean 
ads_eco_mean = as_tibble(zonal(ads_all, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insects") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  left_join(scVeg_zonal) %>%
  group_by(Name, TransitionGroupID) %>%
  summarise(MeanArea = mean(Value), SdArea = sd(Value), Area = mean(Area)) %>%
  mutate(Pct=MeanArea/Area, PctSd=SdArea/Area)

# Create Distributions datasheet
dist_ads_eco_mean = tibble(StratumID = ads_eco_mean$Name,
                            DistributionTypeID = "Insect: Historical Mean",
                            ExternalVariableTypeID = "Insect",
                            ExternalVariableMin = 1998,
                            ExternalVariableMax = 2016,
                            Value = ads_eco_mean$Pct) %>%
  write_csv("data/distributions/distribution-insects-ecoregion-historical-mean.csv")







# Annual Insect Probability by Timestep (Annual Probability Multiplier) -----------------

ads_temporal_total = as_tibble(zonal(ads_all, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  left_join(scVeg_zonal) %>%
  group_by(Timestep, Name, TransitionGroupID) %>%
  summarise(TotalAnnualArea=sum(Value), Area=sum(Area)) %>%
  group_by(Name) %>%
  mutate(MeanAnnualArea = mean(TotalAnnualArea)) %>%
  mutate(RelativeMultiplier = TotalAnnualArea/MeanAnnualArea)

dist_ads_temporal_total = tibble(StratumID = ads_temporal_total$Name,
                                  DistributionTypeID = "Insect: Annual Variability",
                                  ExternalVariableTypeID = "Insect",
                                  ExternalVariableMin = ads_temporal_total$Timestep,
                                  ExternalVariableMax = ads_temporal_total$Timestep,
                                  Value = ads_temporal_total$RelativeMultiplier) %>%
  write_csv("data/distributions/distribution-insects-historical-ecoregion-variability.csv")







# Fire Severity Probability by Ecoregion (Fire Severity Multiplier) --------------------


# High Severity

ecoregion_high_mean = as_tibble(zonal(adsHigh, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: High Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID,  TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)

# Medium Severity

ecoregion_med_mean = as_tibble(zonal(adsMed, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: Medium Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID,  TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)

# Low Severity

ecoregion_low_mean = as_tibble(zonal(adsLow, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "X"))) %>%
  mutate(TransitionGroupID = "Insect: Low Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID,  TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)



# Merge Severities together
ecoregion_severity_mean = bind_rows(ecoregion_high_mean, ecoregion_med_mean, ecoregion_low_mean) %>%
  group_by(Name) %>%
  mutate(Total = sum(Mean)) %>%
  mutate(Prob = ifelse(Total==0, 0, Mean/Total)) %>%
  arrange(Name)

dist_ecoregion_severity_mean = tibble(StratumID = ecoregion_severity_mean$Name,
                                      DistributionTypeID = ecoregion_severity_mean$TransitionGroupID,
                                      ExternalVariableTypeID = "Insect",
                                      ExternalVariableMin = 1998,
                                      ExternalVariableMax = 2016,
                                      Value = ecoregion_severity_mean$Prob) %>%
  write_csv("data/distributions/distribution-insects-historical-severity.csv")




# Calculate Size Distribution ---------------------------------------------

# Use the Insect rasters to create age distribution
df = lsm_p_area(ads_all, directions = 8) %>% filter(class == 1)

df1 = df %>%
  rename("Hectares" = "value") %>%
  arrange(-Hectares) %>%
  mutate(MinSize = if_else(Hectares>=1 & Hectares<500, 1, 
                           if_else(Hectares>=500 & Hectares<1000, 500, 
                                   if_else(Hectares>=1000 & Hectares<2000, 1000, 
                                           if_else(Hectares>=2000 & Hectares<5000, 2000, 
                                                   if_else(Hectares>=5000 & Hectares<10000, 5000, 
                                                           if_else(Hectares>=10000 & Hectares<20000, 10000, 
                                                                   if_else(Hectares>=20000 & Hectares<50000, 20000, 
                                                                           if_else(Hectares>=50000 & Hectares<100000, 50000, 
                                                                                   if_else(Hectares>=100000 & Hectares<200000, 100000, 
                                                                                           if_else(Hectares>=200000 & Hectares<500000, 200000, 0)))))))))), 
         MaxSize = if_else(Hectares>=1 & Hectares<500, 500, 
                           if_else(Hectares>=500 & Hectares<1000, 1000, 
                                   if_else(Hectares>=1000 & Hectares<2000, 2000, 
                                           if_else(Hectares>=2000 & Hectares<5000, 5000, 
                                                   if_else(Hectares>=5000 & Hectares<10000, 10000, 
                                                           if_else(Hectares>=10000 & Hectares<20000, 20000, 
                                                                   if_else(Hectares>=20000 & Hectares<50000, 50000, 
                                                                           if_else(Hectares>=50000 & Hectares<100000, 100000, 
                                                                                   if_else(Hectares>=100000 & Hectares<200000, 200000, 
                                                                                           if_else(Hectares>=200000 & Hectares<500000, 500000, 0)))))))))))

df2 = df1 %>%
  group_by(MinSize, MaxSize) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(freq = n / sum(n))


insect_size_distribution = data.frame(Timestep = 2017,
                                    TransitionGroupID = "Insect",
                                    MaximumArea = df2$MaxSize,
                                    RelativeAmount = df2$freq)
write_csv(insect_size_distribution, "data/size-distribution/size-distribution-insect.csv")

