
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
library(landscapemetrics)


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


# Read in State Class Map
sc = raster("data/initial-conditions/ic-state-class.tif")

# Calculate the natural vegetation area for each ecoregion
scVeg = reclassify(sc, c(0,32,0, 33,79,1, 80,99,0, 100,999,1))
scVeg_zonal = data.frame(zonal(scVeg, ecoregions, sum)) %>%
  rename("ID"="zone", "Area"="value") %>%
  mutate(Area = Area*100)


# Define Fire Types to be included and create raster stack----------------------------------------

# Fire types
fire_types_all = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity %in% c("High", "Medium", "Low"))
fire_types_high = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "High")
fire_types_med = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Medium")
fire_types_low = rat %>% filter(Dist_Type %in% c("Wildfire", "Wildland Fire Use", "Wildland Fire", "Prescribed Fire"), Severity == "Low")


# Create a raster stack from each years Landfire disturbance output
files = paste0(dirs, "/", list.files(dirs, pattern = "*.tif$", recursive = T))[1:18]
rstack = stack(files)




# Average Fire Probability by Ecoregion (Base Fire Probability) ----------------------------------------------------------

# Create stack and reclassify rasters to include all fires
rc = fire_types_all %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_all = reclassify(rstack, rcl = rc)
fire_all = reclassify(fire_all, c(-1.5,-0.5,1, 0,Inf,0))


# Ecoregion Mean 
fire_eco_mean = as_tibble(zonal(fire_all, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  left_join(scVeg_zonal) %>%
  group_by(Name, TransitionGroupID) %>%
  summarise(MeanBurnArea = mean(Value), SdBurnArea = sd(Value), Area = mean(Area)) %>%
  mutate(Pct=MeanBurnArea/Area, PctSd=SdBurnArea/Area)

# Create Distributions datasheet
dist_fire_eco_mean = tibble(StratumID = fire_eco_mean$Name,
                            DistributionTypeID = "Fire: Historical Mean",
                            ExternalVariableTypeID = "Fire",
                            ExternalVariableMin = 1999,
                            ExternalVariableMax = 2016,
                            Value = fire_eco_mean$Pct,
                            ValueDistributionTypeID = "Normal",
                            ValueDistributionFrequency = "Iteration and Timestep",
                            ValueDistributionSD = fire_eco_mean$PctSd) %>%
  write_csv("data/distributions/distribution-fire-ecoregion-historical-mean.csv")






# Annual Fire Probability by Timestep (Annual Probability Multiplier) -----------------

fire_temporal_total = as_tibble(zonal(fire_all, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire") %>%
  rename("ID"="zone") %>%
  left_join(ecoregion_df) %>%
  left_join(scVeg_zonal) %>%
  group_by(Timestep, Name, TransitionGroupID) %>%
  summarise(TotalAnnualBurnArea=sum(Value), Area=sum(Area)) %>%
  ungroup() %>%
  mutate(MeanAnnualBurnArea = mean(TotalAnnualBurnArea)) %>%
  mutate(RelativeMultiplier = TotalAnnualBurnArea/MeanAnnualBurnArea)

dist_fire_temporal_total = tibble(StratumID = fire_temporal_total$Name,
                                  DistributionTypeID = "Fire: Annual Variability",
                                  ExternalVariableTypeID = "Fire",
                                  ExternalVariableMin = fire_temporal_total$Timestep,
                                  ExternalVariableMax = fire_temporal_total$Timestep,
                                  Value = fire_temporal_total$RelativeMultiplier) %>%
  write_csv("data/distributions/distribution-fire-historical-ecoregion-variability.csv")






# Fire Severity Probability by Ecoregion (Fire Severity Multiplier) --------------------


# High Severity
rc = fire_types_high %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_high = reclassify(rstack, rcl = rc)
fire_high = reclassify(fire_high, c(-1.5,-0.5,1, 0,Inf,0))

ecoregion_high_mean = as_tibble(zonal(fire_high, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: High Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID, TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)

# Medium Severity
rc = fire_types_med %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_med = reclassify(rstack, rcl = rc)
fire_med = reclassify(fire_med, c(-1.5,-0.5,1, 0,Inf,0))

ecoregion_med_mean = as_tibble(zonal(fire_med, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: Medium Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID, TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)

# Low Severity
rc = fire_types_low %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
fire_low = reclassify(rstack, rcl = rc)
fire_low = reclassify(fire_low, c(-1.5,-0.5,1, 0,Inf,0))

ecoregion_low_mean = as_tibble(zonal(fire_low, ecoregions, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Fire: Low Severity") %>%
  rename("ID"="zone") %>%
  group_by(ID, TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(ecoregion_df)



# Merge Severities together
ecoregion_severity_mean = bind_rows(ecoregion_high_mean, ecoregion_med_mean, ecoregion_low_mean) %>%
  group_by(Name) %>%
  mutate(Total = sum(Mean)) %>%
  mutate(Prob = Mean/Total) %>%
  arrange(Name)

dist_ecoregion_severity_mean = tibble(StratumID = ecoregion_severity_mean$Name,
                                      DistributionTypeID = ecoregion_severity_mean$TransitionGroupID,
                                      ExternalVariableTypeID = "Fire",
                                      ExternalVariableMin = 1999,
                                      ExternalVariableMax = 2016,
                                      Value = ecoregion_severity_mean$Prob) %>%
  write_csv("data/distributions/distribution-fire-historical-severity.csv")






# Fire Size Distribution --------------------------------------------------

# MTBS data downloaded from https://www.mtbs.gov/direct-download on 2020-06-10
ca_bound = read_sf("I:/GIS-Vector/Boundary and Census/Tiger/tl_2009_us_state_conus.shp") %>%
  filter(NAME == "California")

# Define Size Class Bins
size_classes = tibble(min = c(1,501,1001,1501,2001,2501,5001,10001,20001,30001,40001,50001,60001,70001,80001,90001,100001,200001),
                      max = c(500,1000,1500,2000,2500,5000,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,200000,250000))

mtbs_perim = read_sf("I:/GIS-Vector/MTBS/mtbs_perimeter_data/mtbs_perims_DD.shp")
mtbs_perim = st_intersection(mtbs_perim, ca_bound)

mtbs_perim1 = mtbs_perim %>% arrange(-Acres) %>%
  mutate(Hectares = Acres * 0.404686) %>%
  mutate(MinSize = if_else(Hectares>=1 & Hectares<500, 1, 0), MaxSize = if_else(Hectares>=1 & Hectares<500, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=500 & Hectares<1000, 500, 0), MaxSize = if_else(Hectares>=500 & Hectares<1000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=1000 & Hectares<2000, 1000, 0), MaxSize = if_else(Hectares>=1000 & Hectares<2000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=2000 & Hectares<5000, 2000, 0), MaxSize = if_else(Hectares>=2000 & Hectares<5000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=5000 & Hectares<10000, 5000, 0), MaxSize = if_else(Hectares>=5000 & Hectares<10000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=10000 & Hectares<20000, 10000, 0), MaxSize = if_else(Hectares>=10000 & Hectares<20000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=20000 & Hectares<50000, 20000, 0), MaxSize = if_else(Hectares>=20000 & Hectares<50000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=50000 & Hectares<100000, 50000, 0), MaxSize = if_else(Hectares>=50000 & Hectares<100000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=100000 & Hectares<200000, 100000, 0), MaxSize = if_else(Hectares>=100000 & Hectares<200000, 500, 0)) %>%
  mutate(MinSize = if_else(Hectares>=200000 & Hectares<500000, 200000, 0), MaxSize = if_else(Hectares>=200000 & Hectares<500000, 500, 0))

mtbs_perim1 = mtbs_perim %>% 
  as_tibble() %>%
  dplyr::select(-geometry) %>%
  arrange(-Acres) %>%
  mutate(Hectares = Acres * 0.404686) %>%
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



mtbs_perim2 = mtbs_perim1 %>%
  group_by(MinSize, MaxSize) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(freq = n / sum(n))







df = lsm_p_area(fire_all, directions = 8) %>% filter(class == 1)

df1 = df %>%
  rename("Hectares" = "value") %>%
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
  group_by(maxsize) %>%
  summarise(freq=sum(class)) %>%
  mutate(pct = freq/sum(freq))
  
hist(df1$maxsize)



