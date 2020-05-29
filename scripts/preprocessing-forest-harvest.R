
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
clearcut_types = rat %>% filter(Dist_Type %in% c("Clearcut", "Harvest", "Mastication", "Other Mechanical"), Severity %in% c("High", "Medium"))
selection_types = rat %>% filter(Dist_Type %in% c("Thinning"))

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




# Zonal Summaries and Historical Distributions ---------------------------------------------------------

# Clearcut
clearcut_zonal = as_tibble(zonal(clearcut, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Management: Forest Clearcut [Type]") %>%
  rename("ID"="zone") %>%
  left_join(counties_df) %>%
  mutate(DistributionTypeID = "Management: Forest Clearcut",
         ExternalVariableTypeID = "Forest Harvest",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Timestep and iteration",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(clearcut_zonal, "data/distributions/distribution-clearcut.csv")

# Calculate Mean across all years
clearcut_zonal_mean = clearcut_zonal %>%
  group_by(PrimaryStratumID) %>%
  summarise(Mean=mean(Value, na.rm=T), Sd=sd(Value, na.rm=T), Min=min(Value, na.rm=T), Max=max(Value, na.rm=T))

# Plot county historical totals and check for anomolies 
ggplot(clearcut_zonal, aes(x=ExternalVariableMin, y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~PrimaryStratumID)


# Selection
selection_zonal = as_tibble(zonal(selection, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "Timestep", values_to = "Value") %>%
  mutate(Value = Value*100) %>%
  mutate(Timestep = as.numeric(str_remove(Timestep, pattern = "landfire.disturbance."))) %>%
  mutate(TransitionGroupID = "Management: Forest Selection [Type]") %>%
  rename("ID"="zone") %>%
  left_join(counties_df) %>%
  mutate(DistributionTypeID = "Management: Forest Selection",
         ExternalVariableTypeID = "Forest Harvest",
         ExternalVariableMin = Timestep,
         ExternalVariableMax = Timestep,
         Value = Value,
         ValueDistributionTypeID = "Normal",
         ValueDistributionFrequency = "Timestep and iteration",
         ValueDistributionSD = Value*0.5) %>%
  dplyr::select(PrimaryStratumID=Name, DistributionTypeID, ExternalVariableTypeID, ExternalVariableMin, ExternalVariableMax, Value, DistributionTypeID, ValueDistributionFrequency, ValueDistributionSD)
write_csv(selection_zonal, "data/distributions/distribution-selection.csv")

# Calculate Mean across all years
selection_zonal_mean = selection_zonal %>%
  group_by(PrimaryStratumID) %>%
  summarise(Mean=mean(Value, na.rm=T), Sd=sd(Value, na.rm=T), Min=min(Value, na.rm=T), Max=max(Value, na.rm=T))

# Plot county historical totals and check for anomolies 
ggplot(selection_zonal, aes(x=ExternalVariableMin, y=Value)) +
  geom_bar(stat="identity") +
  facet_wrap(~PrimaryStratumID)





# Clearcut SSP Projections ---------------------------------------------------------

# SSP2
clearcut_ssp2 = clearcut_zonal_mean %>%
  mutate(scenario = "ssp2") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = Mean*change_mult,
         SdMult = Sd*change_mult,
         MinMult = Min*change_mult,
         MaxMult = Max*change_mult) %>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 

# Create a transition targets file
clearcut_ssp2_targets = tibble(SecondaryStratumID = clearcut_ssp2$PrimaryStratumID,
                               Timestep = clearcut_ssp2$Timestep,
                               TransitionGroupID = "Management: Forest Clearcut",
                               Amount = clearcut_ssp2$MeanMult, 
                               DistributionType = "Normal",
                               DistributionFrequencyID = "Iteration and Timestep",
                               DistributionSD = clearcut_ssp2$SdMult,
                               DistributionMin = clearcut_ssp2$MinMult,
                               DistributionMax = clearcut_ssp2$MaxMult)
write_csv(clearcut_ssp2_targets, "data/transition-targets/transition-targets-clearcut-ssp2.csv")

# SSP5
clearcut_ssp5 = clearcut_zonal_mean %>%
  mutate(scenario = "ssp5") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = Mean*change_mult,
         SdMult = Sd*change_mult,
         MinMult = Min*change_mult,
         MaxMult = Max*change_mult)%>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 

# Create a transition targets file
clearcut_ssp5_targets = tibble(SecondaryStratumID = clearcut_ssp5$PrimaryStratumID,
                               Timestep = clearcut_ssp5$Timestep,
                               TransitionGroupID = "Management: Forest Clearcut",
                               Amount = clearcut_ssp5$MeanMult, 
                               DistributionType = "Normal",
                               DistributionFrequencyID = "Iteration and Timestep",
                               DistributionSD = clearcut_ssp5$SdMult,
                               DistributionMin = clearcut_ssp5$MinMult,
                               DistributionMax = clearcut_ssp5$MaxMult)
write_csv(clearcut_ssp5_targets, "data/transition-targets/transition-targets-clearcut-ssp5.csv")






# Selection SSP Projections ---------------------------------------------------------------

#SSP2
selection_ssp2 = selection_zonal_mean %>%
  mutate(scenario = "ssp2") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = Mean*change_mult,
         SdMult = Sd*change_mult,
         MinMult = Min*change_mult,
         MaxMult = Max*change_mult) %>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 

# Create a transition targets file
selection_ssp2_targets = tibble(SecondaryStratumID = selection_ssp2$PrimaryStratumID,
                                Timestep = selection_ssp2$Timestep,
                                TransitionGroupID = "Management: Forest Selection",
                                Amount = selection_ssp2$MeanMult, 
                                DistributionType = "Normal",
                                DistributionFrequencyID = "Iteration and Timestep",
                                DistributionSD = selection_ssp2$SdMult,
                                DistributionMin = selection_ssp2$MinMult,
                                DistributionMax = selection_ssp2$MaxMult)
write_csv(selection_ssp2_targets, "data/transition-targets/transition-targets-selection-ssp2.csv")

# SSP5
selection_ssp5 = selection_zonal_mean %>%
  mutate(scenario = "ssp5") %>%
  left_join(zonal_ssp_mean, by="scenario") %>%
  mutate(MeanMult = Mean*change_mult,
         SdMult = Sd*change_mult,
         MinMult = Min*change_mult,
         MaxMult = Max*change_mult)%>%
  filter(year>=2020) %>%
  mutate(Timestep = if_else(year==2020, 2017, year-9)) 

selection_ssp = bind_rows(selection_ssp2, selection_ssp5) 

# Create a transition targets file
selection_ssp5_targets = tibble(SecondaryStratumID = selection_ssp5$PrimaryStratumID,
                                Timestep = selection_ssp5$Timestep,
                                TransitionGroupID = "Management: Forest Selection",
                                Amount = selection_ssp5$MeanMult, 
                                DistributionType = "Normal",
                                DistributionFrequencyID = "Iteration and Timestep",
                                DistributionSD = selection_ssp5$SdMult,
                                DistributionMin = selection_ssp5$MinMult,
                                DistributionMax = selection_ssp5$MaxMult)
write_csv(selection_ssp5_targets, "data/transition-targets/transition-targets-selection-ssp5.csv")






# Data for Plotting -------------------------------------------------------

# Calculate totals across state for plotting
clearcut_ssp = bind_rows(clearcut_ssp2, clearcut_ssp5) 

# Calculate totals state-wide

# Clearcut
clearcut_ssp_state = clearcut_ssp %>%
  group_by(year, Timestep, scenario) %>%
  summarise(MeanMult=sum(MeanMult), SdMult=sum(SdMult), MinMult=sum(MinMult), MaxMult=sum(MaxMult))

# Selection
selection_ssp_state = selection_ssp %>%
  group_by(year, Timestep, scenario) %>%
  summarise(MeanMult=sum(MeanMult), SdMult=sum(SdMult), MinMult=sum(MinMult), MaxMult=sum(MaxMult))

# Plot Clearcut
ggplot(clearcut_ssp, aes(x=Timestep, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=if_else(MeanMult-SdMult<0,0, MeanMult-SdMult), ymax=MeanMult+SdMult), alpha=0.3) +
  geom_line() +
  geom_point() +
  facet_wrap(~PrimaryStratumID)

# Plot Selection
ggplot(selection_ssp, aes(x=Timestep, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=if_else(MeanMult-SdMult<0,0, MeanMult-SdMult), ymax=MeanMult+SdMult), alpha=0.3) +
  geom_line() +
  geom_point() +
  facet_wrap(~PrimaryStratumID)
