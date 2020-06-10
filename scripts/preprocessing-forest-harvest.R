
# Header ------------------------------------------------------------------
# Code to calculate annual forest harvest rates from the Landfire Disturbance
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions of harvest disturbance (clearcut and selection harvest) and future projections based on SSP scenarios
# Landfire Disturbance data can be downloaded here: https://www.landfire.gov/disturbance_2.php
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)

# Read in ICLUS SSP projections
# source(preprocessing-iclus-ssp.R)
zonal_ssp = read_csv("docs/ssp/iclus-ssp-zonal-county-summary.csv")

# Calculate the total state-wide change multipliers from SSP projections by decade
zonal_ssp_mean = zonal_ssp %>%
  group_by(year, scenario) %>%
  summarise(ref_annual_change=sum(ref_annual_change), annual_change=sum(annual_change)) %>%
  mutate(change_mult = annual_change/ref_annual_change) 


# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")

# Read in California Ecoregions raster
ecoregions = raster("data/initial-conditions/ic-ecoregion.tif")
ecoregion_df = read_csv("data/definitions/ecoregions.csv")


# Get a Landfire attribute table
dbf = list.files(paste0(dirs[1]), pattern = "*.dbf$", recursive = T)
rat = read.dbf(paste0(dirs[1], "/", dbf)[2]) %>% as_tibble()

# Get a list of the disturbance types
dist_type = unique(rat$Dist_Type)

# Harvest types
clearcut_types = rat %>% filter(Dist_Type %in% c("Clearcut", "Harvest", "Mastication", "Other Mechanical", "Unknown"), Severity %in% c("High", "Medium"))
selection_types = rat %>% filter(Dist_Type %in% c("Thinning"))
harvest_types = rat %>% filter(Dist_Type %in% c("Clearcut", "Harvest", "Mastication", "Other Mechanical", "Unknown","Thinning"), Severity %in% c("High", "Medium"))

# Create a raster stack for each years Landfire disturbance output
files = paste0(dirs, "/", list.files(dirs, pattern = "*.tif$", recursive = T))[1:18]
rstack = stack(files)





# Create Spatial Multipliers -------------------------------------

#  Clearcut
outdir = "data/spatial-multipliers/clearcut/"
type = "clearcut"
rc = clearcut_types %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
clearcut = reclassify(rstack, rcl = rc)
clearcut = reclassify(clearcut, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(clearcut, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")


# Selection 
outdir = "data/spatial-multipliers/selection/"
type = "selection"
rc = selection_types %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
selection = reclassify(rstack, rcl = rc)
selection = reclassify(selection, c(-1.5,-0.5,1, 0,Inf,0))

# Write out spatial multipliers
writeRaster(selection, paste0(outdir, type, "-", seq(1999,2016), ".tif"), format="GTiff", bylayer=T, overwrite=T, options="COMPRESS=DEFLATE", datatype="INT1U")





# Average Harvest Amount by County (Average Harvest Amount) -----------------

# All Harvest
# rc = harvest_types %>% dplyr::select(from=Value) %>% mutate(from=from-0.4, to=from+0.9, becomes=-1)
# harvest = reclassify(rstack, rcl = rc)
# harvest = reclassify(harvest, c(-1.5,-0.5,1, 0,Inf,0))
harvest = clearcut + selection
names(harvest) = names(clearcut)

harvest_cty_mean = as_tibble(zonal(harvest, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Harvest") %>%
  rename("ID"="zone") %>%
  left_join(counties_df) %>%
  group_by(Name, TransitionGroupID) %>%
  summarise(MeanArea = mean(Value), SdArea = sd(Value))

# Create Distributions datasheet
dist_harvest_cty_mean = tibble(SecondaryStratumID = harvest_cty_mean$Name,
                            DistributionTypeID = "Harvest: Historical Mean",
                            ExternalVariableTypeID = "Forest Harvest",
                            ExternalVariableMin = 1999,
                            ExternalVariableMax = 2016,
                            Value = harvest_cty_mean$MeanArea,
                            ValueDistributionTypeID = "Normal",
                            ValueDistributionFrequency = "Iteration and Timestep",
                            ValueDistributionSD = harvest_cty_mean$SdArea)
dist_harvest_cty_mean = dist_harvest_cty_mean %>%
  mutate(ValueDistributionSD = ifelse(ValueDistributionSD==0, NA, ValueDistributionSD)) %>%
  mutate(ValueDistributionTypeID = ifelse(ValueDistributionSD==0, NA, ValueDistributionTypeID)) %>%
  mutate(ValueDistributionFrequency = ifelse(ValueDistributionSD==0, NA, ValueDistributionFrequency))

write_csv(dist_harvest_cty_mean, "data/distributions/distribution-harvest-county-historical-mean.csv")






# Annual Harvest Probability by Timestep (Annual Probability Multiplier) -----------------

harvest_temporal_total = as_tibble(zonal(harvest, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Harvest") %>%
  rename("ID"="zone") %>%
  left_join(counties_df) %>%
  group_by(Timestep, Name, TransitionGroupID) %>%
  summarise(TotalAnnualArea=sum(Value)) %>%
  group_by(Name) %>%
  mutate(MeanAnnualArea = mean(TotalAnnualArea)) %>%
  mutate(RelativeMultiplier = ifelse(TotalAnnualArea==0, 0, TotalAnnualArea/MeanAnnualArea))

dist_harvest_temporal_total = tibble(SecondaryStratumID = harvest_temporal_total$Name,
                                  DistributionTypeID = "Harvest: Annual Variability",
                                  ExternalVariableTypeID = "Forest Harvest",
                                  ExternalVariableMin = harvest_temporal_total$Timestep,
                                  ExternalVariableMax = harvest_temporal_total$Timestep,
                                  Value = harvest_temporal_total$RelativeMultiplier) %>%
  write_csv("data/distributions/distribution-harvest-historical-county-variability.csv")








# Harvest Type Probability by Ecoregion (Harvest Type Multiplier) --------------------


# Clearcut

ecoregion_clearcut_mean = as_tibble(zonal(clearcut, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Harvest: Forest Clearcut") %>%
  rename("ID"="zone") %>%
  group_by(ID, TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(counties_df)

# Selection

ecoregion_selection_mean = as_tibble(zonal(selection, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Harvest: Forest Selection") %>%
  rename("ID"="zone") %>%
  group_by(ID, TransitionGroupID) %>%
  summarise(Mean = mean(Value)) %>%
  left_join(counties_df)



# Merge Harvest together
county_harvest_mean = bind_rows(ecoregion_clearcut_mean, ecoregion_selection_mean) %>%
  group_by(Name) %>%
  mutate(Total = sum(Mean)) %>%
  mutate(Prob = ifelse(Total==0, 0, Mean/Total)) %>%
  arrange(Name) %>%
  group_by(Name) 

dist_county_harvest_mean = tibble(SecondaryStratumID = county_harvest_mean$Name,
                                      DistributionTypeID = county_harvest_mean$TransitionGroupID,
                                      ExternalVariableTypeID = "Forest Harvest",
                                      ExternalVariableMin = 1999,
                                      ExternalVariableMax = 2016,
                                      Value = county_harvest_mean$Prob) %>%
  write_csv("data/distributions/distribution-harvest-historical-type.csv")


























# Clearcut SSP Projections ---------------------------------------------------------



# SSP2
harvest_ssp2 = harvest_cty_mean %>%
  mutate(scenario = "ssp2") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = MeanArea*change_mult,
         SdMult = SdArea*change_mult) %>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 

# SSP5
harvest_ssp5 = harvest_cty_mean %>%
  mutate(scenario = "ssp5") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = MeanArea*change_mult,
         SdMult = SdArea*change_mult) %>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 





# Create a transition targets file
harvest_ssp2_targets = tibble(SecondaryStratumID = harvest_ssp2$Name,
                               Timestep = harvest_ssp2$Timestep,
                               TransitionGroupID = "Management: Forest Clearcut [Type]",
                               Amount = harvest_ssp2$MeanMult, 
                               DistributionType = "Normal",
                               DistributionFrequencyID = "Iteration and Timestep",
                               DistributionSD = harvest_ssp2$SdMult)
write_csv(harvest_ssp2_targets, "data/transition-targets/transition-targets-harvest-ssp2.csv")



# Create a transition targets file
harvest_ssp5_targets = tibble(SecondaryStratumID = harvest_ssp5$Name,
                               Timestep = harvest_ssp5$Timestep,
                               TransitionGroupID = "Management: Forest Clearcut [Type]",
                               Amount = harvest_ssp5$MeanMult, 
                               DistributionType = "Normal",
                               DistributionFrequencyID = "Iteration and Timestep",
                               DistributionSD = harvest_ssp5$SdMult)
write_csv(harvest_ssp5_targets, "data/transition-targets/transition-targets-harvest-ssp5.csv")











# Data for Plotting -------------------------------------------------------

# Calculate totals across state for plotting
harvest_ssp = bind_rows(harvest_ssp2, harvest_ssp5) 

# Plot Harvest
ggplot(harvest_ssp, aes(x=Timestep, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=if_else(MeanMult-SdMult<0,0, MeanMult-SdMult), ymax=MeanMult+SdMult), alpha=0.3) +
  geom_line() +
  geom_point() +
  facet_wrap(~Name)


