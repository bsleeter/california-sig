#preprocessing-iclues data

library(raster)
library(sf)
library(tidyverse)
library(readxl)

# Read in counties table
counties_tbl = read_csv("data/definitions/counties.csv")

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")

# Read in FMMP data
mergedData_Long = read_csv("docs/fmmp/fmmp-conversion-totals.csv")

# Filter urbanization
urbanGain = mergedData_Long %>%
  filter(To_Class == "Urban", From_Class == "Total") %>%
  mutate(Net = "Gain")
urbanGain$FromYear = as.numeric(urbanGain$FromYear)
urbanGain$ToYear = as.numeric(urbanGain$ToYear)

# Create Urbanization Historical Distributions datasheet
dist_urb_hist = tibble(SecondaryStratumID = urbanGain$County,
                       DistributionTypeID = "Historical Rate: Urbanization",
                       ExternalVariableTypeID = "Historical Year: Urbanization",
                       ExternalVariableMin = urbanGain$ToYear-1,
                       ExternalVariableMax = urbanGain$ToYear,
                       Value = urbanGain$Hectares,
                       ValueDistributionTypeID = "Normal",
                       ValueDistributionFrequency = "Timestep and iteration",
                       ValueDistributionSD = urbanGain$Hectares*0.5)
dist_urb_hist = dist_urb_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_urb_hist, "data/distributions/distribution-urbanization.csv")

# Create mean historical urbanization for each county
urbanGainMean = urbanGain %>%
  group_by(County) %>%
  summarise(Mean=mean(Hectares, na.rm=T), Sd=sd(Hectares, na.rm=T), Min=min(Hectares, na.rm=T), Max=max(Hectares, na.rm=T))
urbanGainMean  



# # Create historical distributions input datasheet
# # Historical FMMP assumes a SD equal to half of the mean. Min and max are calculated as 2* the Sd. These are used to reflect annual variability.
# hist_urb_nodata = tibble(SecondaryStratumID = rep(c("Alpine", "Calaveras", "Del Norte", "Humboldt", "Inyo", "Kings", "Lassen", "Mono", "Plumas", "San Francisco", "Trinity", "Tuolumne"),8),
#                          Timestep = sort(rep(c(2002,2004,2006,2008,2010,2012,2014,2016),12)),
#                          TransitionGroupID = "Urbanization",
#                          Amount = 0)







###### ICLUS SSP Projections ###### 
# SSP2
ssp2_list = list.files(path = "I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc", pattern = "*.tif$")
ssp2_hist = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc/", ssp2_list[1:2]))
ssp2_proj = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc/", ssp2_list[3:11]))
ssp2_hist = projectRaster(ssp2_hist, counties, method="ngb")
ssp2_hist = mask(ssp2_hist, counties)
ssp2_proj = projectRaster(ssp2_proj, counties, method="ngb")
ssp2_proj = mask(ssp2_proj, counties)
ssp2 = stack(ssp2_hist, ssp2_proj)
ssp2 = reclassify(ssp2, c(-Inf,11.5,0, 11.6,16.5,1, 16.6,17.5,0, 17.6,Inf,0))

zonal_ssp2 = data.frame(zonal(ssp2, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "year", values_to = "area") %>%
  mutate(year = str_replace(year, pattern = "_ssp2_nocc", replacement = "")) %>%
  mutate(year = str_remove(year, pattern = "ICLUS_v2.1_land_use_conus_")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(scenario = "ssp2")

# SSP5
ssp5_list = list.files(path = "I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc", pattern = "*.tif$")
ssp5_hist = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc/", ssp5_list[1:2]))
ssp5_proj = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc/", ssp5_list[3:11]))
ssp5_hist = projectRaster(ssp5_hist, counties, method="ngb")
ssp5_hist = mask(ssp5_hist, counties)
ssp5_proj = projectRaster(ssp5_proj, counties, method="ngb")
ssp5_proj = mask(ssp5_proj, counties)
ssp5 = stack(ssp5_hist, ssp5_proj)
ssp5 = reclassify(ssp5, c(-Inf,11.5,0, 11.6,16.5,1, 16.6,17.5,0, 17.6,Inf,0))

zonal_ssp5 = data.frame(zonal(ssp5, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "year", values_to = "area") %>%
  mutate(year = str_replace(year, pattern = "_ssp5_nocc", replacement = "")) %>%
  mutate(year = str_remove(year, pattern = "ICLUS_v2.1_land_use_conus_")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(scenario = "ssp5")


# Merge SSP zonal summaries
zonal_ssp_ref = bind_rows(zonal_ssp2, zonal_ssp5) %>%
  group_by(zone, scenario) %>%
  arrange(year) %>%
  mutate(ref_annual_change = (area-lag(area))/10) %>%
  mutate(ref_annual_change = if_else(ref_annual_change<=0, 0.01, ref_annual_change)) %>%
  filter(year==2010) %>%
  ungroup() %>%
  dplyr::select(zone, ref_annual_change)


# Combine with FMMP data to calculate transition target files
zonal_ssp = bind_rows(zonal_ssp2, zonal_ssp5) %>%
  group_by(zone, scenario) %>%
  arrange(zone, scenario, year) %>%
  mutate(annual_change = (area-lag(area))/10) %>%
  mutate(annual_change = if_else(annual_change<0,0, annual_change)) %>%
  filter(year>=2010) %>%
  right_join(zonal_ssp_ref) %>%
  mutate(change_mult = annual_change/ref_annual_change) %>%
  dplyr::select(ID = zone, year, scenario, ref_annual_change, annual_change, change_mult) %>%
  left_join(counties_tbl) %>%
  rename("County"="Name") %>%
  ungroup() %>%
  dplyr::select(County, year, scenario, ref_annual_change, annual_change, change_mult)

# Merge with FMMP and create projection targets
ssp_fmmp = zonal_ssp %>%
  left_join(urbanGainMean) %>%
  filter(year>=2020) %>%
  mutate(MeanMult = if_else(is.na(Mean), annual_change*100, Mean*change_mult),
         SdMult = if_else(is.na(Mean), annual_change*0.5*100, Sd*change_mult),
         MinMult = if_else(is.na(Mean), 0, Min*change_mult),
         MaxMult = if_else(is.na(Mean), annual_change*2*100, Max*change_mult))

ggplot(ssp_fmmp, aes(x=year, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=MinMult, ymax=MaxMult), alpha = 0.5) +
  geom_line() +
  geom_point() +
  facet_wrap(~County, scales = "free_y")



# Create SSP2 Transition Targets
transition_urb_ssp2 = ssp_fmmp %>%
  filter(scenario=="ssp2", year>=2020) %>%
  mutate(SecondaryStratumID = County,
         Timestep = if_else(year==2020, 2017, year-10),
         TransitionGroupID = "Urbanization",
         Amount = MeanMult,
         DistributionType = "Normal",
         DistributionFrequencyID = "Timestep and iteration",
         DistributionSD = SdMult,
         DistributionMin = MinMult,
         DistributionMax = MaxMult) %>%
  dplyr::select(Timestep, SecondaryStratumID, TransitionGroupID, Amount, DistributionType, DistributionFrequencyID, DistributionSD, DistributionMin, DistributionMax)
write_csv(transition_urb_ssp2, "data/transition-targets/transition-targets-urbanization-ssp2.csv")

# Create SSP5 Transition Targets
dist_urb_ssp5 = ssp_fmmp %>%
  filter(scenario=="ssp5", year>=2020) %>%
  mutate(SecondaryStratumID = County,
         Timestep = if_else(year==2020, 2017, year-10),
         TransitionGroupID = "Urbanization",
         Amount = MeanMult,
         DistributionType = "Normal",
         DistributionFrequencyID = "Timestep and iteration",
         DistributionSD = SdMult,
         DistributionMin = MinMult,
         DistributionMax = MaxMult) %>%
  dplyr::select(Timestep, SecondaryStratumID, TransitionGroupID, Amount, DistributionType, DistributionFrequencyID, DistributionSD, DistributionMin, DistributionMax)
write_csv(transition_urb_ssp5, "data/transition-targets/transition-targets-urbanization-ssp5.csv")














