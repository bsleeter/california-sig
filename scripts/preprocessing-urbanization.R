# Header ------------------------------------------------------------------
# Code to calculate annual change rates due to urbanization using California'a Farmland Mapping and Monitoring Program data (FMMP)
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions and Transition Targets based on SSP scenarios for use within the LUCAS model
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

# Load libraries
library(raster)
library(sf)
library(tidyverse)


# Read in counties table
counties_tbl = read_csv("data/definitions/counties.csv")

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")


# Read in FMMP data or alternatively, run source script to process FMMP data summaries.
# FMMP data downloaded on 2020-05-22 from https://www.conservation.ca.gov/dlrp/fmmp/Pages/county_info.aspx
# FMMP county summaries available at https://github.com/bsleeter/california-sig/tree/master/docs/fmmp/conversion-tables
# source(preprocessing-fmmp.R)
mergedData_Long = read_csv("docs/fmmp/fmmp-conversion-totals.csv")

# Read in ICLUS SSP Zonal Summaries
# source(preprocessing-iclus-ssp.R)
zonal_ssp = read_csv("docs/ssp/iclus-ssp-zonal-county-summary.csv")



# Historical Urbanization -------------------------------------------------

# Filter urbanization transitions
urbanGain = mergedData_Long %>%
  filter(To_Class == "Urban", From_Class == "Total") %>%
  mutate(Net = "Gain")
urbanGain$FromYear = as.numeric(urbanGain$FromYear)
urbanGain$ToYear = as.numeric(urbanGain$ToYear)

# Create Urbanization Historical Distributions datasheet and write to disk
dist_urb_hist = tibble(SecondaryStratumID = urbanGain$County,
                       DistributionTypeID = "Urbanization",
                       ExternalVariableTypeID = "Urbanization",
                       ExternalVariableMin = urbanGain$ToYear-1,
                       ExternalVariableMax = urbanGain$ToYear,
                       Value = urbanGain$Hectares,
                       ValueDistributionTypeID = "Normal",
                       ValueDistributionFrequency = "Iteration and Timestep",
                       ValueDistributionSD = urbanGain$Hectares*0.5)
dist_urb_hist = dist_urb_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_urb_hist, "data/distributions/distribution-urbanization.csv")

# Create mean historical urbanization for each county
urbanGainMean = urbanGain %>%
  group_by(County) %>%
  summarise(Mean=mean(Hectares, na.rm=T), Sd=sd(Hectares, na.rm=T), Min=min(Hectares, na.rm=T), Max=max(Hectares, na.rm=T))
urbanGainMean  





# Projected Urbanization --------------------------------------------------

# Merge SSP data with FMMP urbanization averages and create projection targets
ssp_fmmp = zonal_ssp %>%
  left_join(urbanGainMean) %>%
  filter(year>=2020) %>%
  mutate(MeanMult = if_else(is.na(Mean), annual_change*100, Mean*change_mult),
         SdMult = if_else(is.na(Mean), annual_change*0.5*100, Sd*change_mult),
         MinMult = if_else(is.na(Mean), 0, Min*change_mult),
         MaxMult = if_else(is.na(Mean), annual_change*2*100, Max*change_mult))

# Plot results by county
ggplot(ssp_fmmp, aes(x=year, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=MinMult, ymax=MaxMult), alpha = 0.5) +
  geom_line() +
  geom_point() +
  facet_wrap(~County, scales = "free_y")


# Create SSP2 Transition Targets and write to disk
transition_urb_ssp2 = ssp_fmmp %>%
  filter(scenario=="ssp2", year>=2020) %>%
  mutate(SecondaryStratumID = County,
         Timestep = if_else(year==2020, 2017, year-9), # Use data from 2010-2020 to begin projections in year 2017
         TransitionGroupID = "Urbanization",
         Amount = MeanMult,
         DistributionType = "Normal",
         DistributionFrequencyID = "Iteration and Timestep",
         DistributionSD = SdMult,
         DistributionMin = MinMult,
         DistributionMax = MaxMult) %>%
  dplyr::select(Timestep, SecondaryStratumID, TransitionGroupID, Amount, DistributionType, DistributionFrequencyID, DistributionSD, DistributionMin, DistributionMax)
write_csv(transition_urb_ssp2, "data/transition-targets/transition-targets-urbanization-ssp2.csv")

# Create SSP5 Transition Targets and write to disk
transition_urb_ssp5 = ssp_fmmp %>%
  filter(scenario=="ssp5", year>=2020) %>%
  mutate(SecondaryStratumID = County,
         Timestep = if_else(year==2020, 2017, year-9), # Use data from 2010-2020 to begin projections in year 2017
         TransitionGroupID = "Urbanization",
         Amount = MeanMult,
         DistributionType = "Normal",
         DistributionFrequencyID = "Iteration and Timestep",
         DistributionSD = SdMult,
         DistributionMin = MinMult,
         DistributionMax = MaxMult) %>%
  dplyr::select(Timestep, SecondaryStratumID, TransitionGroupID, Amount, DistributionType, DistributionFrequencyID, DistributionSD, DistributionMin, DistributionMax)
write_csv(transition_urb_ssp5, "data/transition-targets/transition-targets-urbanization-ssp5.csv")














