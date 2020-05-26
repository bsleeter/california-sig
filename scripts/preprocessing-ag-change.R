
library(raster)
library(sf)
library(tidyverse)
library(readxl)

# Preprocessing script for Ag Change (Expansion and Contraction)

# Read in FMMP data
mergedData_Long = read_csv("docs/fmmp/fmmp-conversion-totals.csv")

# Clean up erroneous records for Siskiyou and Modoc counties
mergedData_Long_clean = mergedData_Long %>%
  mutate(Hectares = ifelse(County == "Siskiyou" & ToYear == "1996" & To_Class == "LI_Farmland" & From_Class == "Grazing", 0, Hectares)) %>%
  mutate(Hectares = ifelse(County == "Modoc" & ToYear == "2016" & From_Class == "Grazing" & To_Class == "LI_Farmland", 0, Hectares)) 

# Create Ag Contraction summary
agContraction = mergedData_Long_clean %>%
  filter(From_Class %in% c("P_Farmland", "SI_Farmland", "U_Farmland", "LI_Farmland"), To_Class %in% c("Other", "Grazing")) %>%
  group_by(FromYear, ToYear, County) %>%
  summarise(Hectares = sum(Hectares)) %>%
  mutate(Transition = "Ag Contraction")

# Create Ag Expansion summary
agExpansion = mergedData_Long_clean %>%
  filter(From_Class %in% c("Other", "Grazing"), To_Class %in% c("P_Farmland", "SI_Farmland", "U_Farmland", "LI_Farmland")) %>%
  group_by(FromYear, ToYear, County) %>%
  summarise(Hectares = sum(Hectares)) %>%
  mutate(Transition = "Ag Expansion")

# Merge data for plotting
agChange = bind_rows(agContraction, agExpansion)
agChange$FromYear = as.numeric(agChange$FromYear)
agChange$ToYear = as.numeric(agChange$ToYear)

# Plot data
ggplot(agChange, aes(x=ToYear, y=Hectares, color=Transition)) +
  geom_line() +
  geom_point() +
  facet_wrap(~County)

# Create Ag Expansion Historical Distributions datasheet
dist_agexp_hist = tibble(SecondaryStratumID = agExpansion$County,
                       DistributionTypeID = "Historical Rate: Urbanization",
                       ExternalVariableTypeID = "Historical Year: Urbanization",
                       ExternalVariableMin = agExpansion$ToYear-1,
                       ExternalVariableMax = agExpansion$ToYear,
                       Value = agExpansion$Hectares,
                       ValueDistributionTypeID = "Normal",
                       ValueDistributionFrequency = "Timestep and iteration",
                       ValueDistributionSD = agExpansion$Hectares*0.5)
dist_agexp_hist = dist_agexp_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_agexp_hist, "data/distributions/distribution-ag-expansion.csv")

# Create Ag Contraction Historical Distributions datasheet
dist_agcon_hist = tibble(SecondaryStratumID = agContraction$County,
                         DistributionTypeID = "Historical Rate: Urbanization",
                         ExternalVariableTypeID = "Historical Year: Urbanization",
                         ExternalVariableMin = agContraction$ToYear-1,
                         ExternalVariableMax = agContraction$ToYear,
                         Value = agContraction$Hectares,
                         ValueDistributionTypeID = "Normal",
                         ValueDistributionFrequency = "Timestep and iteration",
                         ValueDistributionSD = agContraction$Hectares*0.5)
dist_agcon_hist = dist_agcon_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_agcon_hist, "data/distributions/distribution-ag-contraction.csv")


agChangeState = agChange %>%
  filter(ToYear <=2016) %>%
  group_by(FromYear, ToYear, Transition) %>%
  summarise(Hectares = sum(Hectares, na.rm=T)) %>%
  pivot_wider(names_from = Transition, values_from = Hectares) %>%
  rename("AgExpansion" = "Ag Expansion", "AgContraction" = "Ag Contraction") %>%
  mutate(NetChange = AgExpansion - AgContraction) %>%
  pivot_longer(cols = c(-FromYear, -ToYear), names_to = "Transition", values_to = "Hectares")

ggplot(agChangeState, aes(x=ToYear, y=Hectares)) +
  geom_bar(stat="identity") +
  geom_smooth(method = "gam") +
  facet_wrap(~Transition)




# Read in ICLUS SSP Zonal Summaries
zonal_ssp = read_csv("docs/ssp/iclus-ssp-zonal-county-summary.csv")


# Create Future Projection based on SSP Trends using full FMMP Historical Mean
# Ag Expansion full temporal Mean
agExpansion_full = agExpansion %>% 
  group_by(County, Transition) %>%
  summarise(Mean = mean(Hectares, na.rm=T), Sd = sd(Hectares, na.rm=T), Min = min(Hectares, na.rm=T), Max = max(Hectares, na.rm=T))

# Ag Contraction full temporal Mean
agContraction_full = agContraction %>% 
  group_by(County, Transition) %>%
  summarise(Mean = mean(Hectares, na.rm=T), Sd = sd(Hectares, na.rm=T), Min = min(Hectares, na.rm=T), Max = max(Hectares, na.rm=T))

# Join with FMMP Ag Expansion
agE_ssp_fmmp_full = zonal_ssp %>%
  left_join(agExpansion_full) %>%
  mutate(Transition = "AgExpansion") %>%
  filter(year>=2020) %>%
  mutate(MeanMult = if_else(is.na(Mean), annual_change, Mean*change_mult),
         SdMult = if_else(is.na(Mean), annual_change*0.5, Sd*change_mult),
         MinMult = if_else(is.na(Mean), 0, Min*change_mult),
         MaxMult = if_else(is.na(Mean), annual_change*2, Max*change_mult))

# Write Ag Expansion Transition Targets for SSP2
agExpansion_ssp2 = agE_ssp_fmmp_full %>%
  filter(scenario=="ssp2") %>%
  mutate(year = if_else(year==2020, 2017, year-9))

agExpansion_ssp2_tt = tibble(Timestep = agExpansion_ssp2$year,
                             SecondaryStratumID = agExpansion_ssp2$County,
                             TransitionGroupID = "Ag Expansion: Cropland",
                             Amount = agExpansion_ssp2$MeanMult,
                             DistributionFrequencyID = "Iteration and Timestep",
                             DistributionSD = agExpansion_ssp2$SdMult,
                             DistributionMin = agExpansion_ssp2$MinMult,
                             DistributionMax = agExpansion_ssp2$MaxMult)
write_csv(agExpansion_ssp2_tt, "data/transition-targets/transition-targets-ag-expansion-ssp2.csv")

# Write Ag Expansion Transition Targets for SSP5
agExpansion_ssp5 = agE_ssp_fmmp_full %>%
  filter(scenario=="ssp5") %>%
  mutate(year = if_else(year==2020, 2017, year-9))

agExpansion_ssp5_tt = tibble(Timestep = agExpansion_ssp5$year,
                             SecondaryStratumID = agExpansion_ssp5$County,
                             TransitionGroupID = "Ag Expansion: Cropland",
                             Amount = agExpansion_ssp5$MeanMult,
                             DistributionFrequencyID = "Iteration and Timestep",
                             DistributionSD = agExpansion_ssp5$SdMult,
                             DistributionMin = agExpansion_ssp5$MinMult,
                             DistributionMax = agExpansion_ssp5$MaxMult)
write_csv(agExpansion_ssp5_tt, "data/transition-targets/transition-targets-ag-expansion-ssp5.csv")


# Write Ag Contraction Transition Targets for SSP2
agContraction_ssp2 = agC_ssp_fmmp_full %>%
  filter(scenario=="ssp2") %>%
  mutate(year = if_else(year==2020, 2017, year-9))

agContraction_ssp2_tt = tibble(Timestep = agContraction_ssp2$year,
                             SecondaryStratumID = agContraction_ssp2$County,
                             TransitionGroupID = "Ag Contraction: Cropland",
                             Amount = agContraction_ssp2$MeanMult,
                             DistributionFrequencyID = "Iteration and Timestep",
                             DistributionSD = agContraction_ssp2$SdMult,
                             DistributionMin = agContraction_ssp2$MinMult,
                             DistributionMax = agContraction_ssp2$MaxMult)
write_csv(agContraction_ssp2_tt, "data/transition-targets/transition-targets-ag-contraction-ssp2.csv")

# Write Ag Contraction Transition Targets for SSP5
agContraction_ssp5 = agC_ssp_fmmp_full %>%
  filter(scenario=="ssp5") %>%
  mutate(year = if_else(year==2020, 2017, year-9))

agContraction_ssp5_tt = tibble(Timestep = agContraction_ssp5$year,
                               SecondaryStratumID = agContraction_ssp5$County,
                               TransitionGroupID = "Ag Contraction: Cropland",
                               Amount = agContraction_ssp5$MeanMult,
                               DistributionFrequencyID = "Iteration and Timestep",
                               DistributionSD = agContraction_ssp5$SdMult,
                               DistributionMin = agContraction_ssp5$MinMult,
                               DistributionMax = agContraction_ssp5$MaxMult)
write_csv(agContraction_ssp5_tt, "data/transition-targets/transition-targets-ag-contraction-ssp5.csv")




















ggplot(agE_ssp_fmmp_full, aes(x=year, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=if_else(MeanMult-Sd<0,0, MeanMult-Sd), ymax=MeanMult+Sd), alpha=0.5) +
  geom_line() +
  geom_point() +
  facet_wrap(~County, scales = "free_y")

# Join with FMMP Ag Contraction
agC_ssp_fmmp_full = zonal_ssp %>%
  left_join(agContraction_full) %>%
  mutate(Transition = "AgContraction") %>%
  filter(year>=2020) %>%
  mutate(MeanMult = if_else(is.na(Mean), annual_change, Mean),
         SdMult = if_else(is.na(Mean), annual_change*0.5, Sd),
         MinMult = if_else(is.na(Mean), 0, Min),
         MaxMult = if_else(is.na(Mean), annual_change*2, Max))

ggplot(agC_ssp_fmmp_full, aes(x=year, y=MeanMult, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=if_else(MeanMult-Sd<0,0, MeanMult-Sd), ymax=MeanMult+Sd), alpha=0.5) +
  geom_line() +
  geom_point() +
  facet_wrap(~County, scales = "free_y")






df = bind_rows(agE_ssp_fmmp_full, agC_ssp_fmmp_full) %>%
  dplyr::select(County, year, scenario, Transition, MeanMult, SdMult) %>%
  group_by(year, Transition, scenario) %>%
  summarise(Mean=sum(MeanMult), Sd=sum(SdMult)) %>%
  pivot_wider(names_from = Transition, values_from = c(Mean, Sd), names_sep = "_") %>%
  mutate(NetMean = (Mean_AgExpansion - Mean_AgContraction),
         NetSd = (Sd_AgExpansion - Sd_AgContraction))

df


ggplot(df, aes(x=year, y=NetMean, color=scenario, fill=scenario)) +
  geom_ribbon(aes(ymin=NetMean-NetSd, ymax=NetMean+NetSd), alpha=0.5) +
  geom_line() +
  geom_point()  +
  facet_wrap(~County, scales = "free_y")



# Helpers
# unique(mergedData_Long_clean$County)
# mergedData_Long %>% filter(County == "Butte" & ToYear == 2016 & From_Class == "Grazing" & To_Class == "LI_Farmland")
