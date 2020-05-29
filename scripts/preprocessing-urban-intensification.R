

# Header ------------------------------------------------------------------
# Code to calculate urban intensification rates from the National Land Cover Database
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions of urban intensification and future projections based on SSP scenarios
# NLCD data can be downloaded here: https://www.mrlc.gov/data
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------



library(raster)
library(sf)
library(tidyverse)



# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")
counties_df = read_csv("data/definitions/counties.csv") %>% dplyr::select(-Description)
stateclass_df = read_csv("data/definitions/state-class-types.csv")



# Read and project/mask NLCD data
#source("preprocessing-nlcd.R")

# Read in SSP projections
zonal_ssp = read_csv("docs/ssp/iclus-ssp-zonal-county-summary.csv")

# Get a list of state class types for Development
stateclass_dev = stateclass_df %>% filter(ID %in% c(21,22,23,24)) %>%
  dplyr::select(Name, ID)

# Create a list of County IDs
countyIDList = unique(counties$ic.counties)

# Create a transition type lookup table
transtypes = tibble(FromStateClassID = c("Developed: Open Space", "Developed: Open Space", "Developed: Open Space", "Developed: Low Intensity", "Developed: Low Intensity" ,"Developed: Medium Intensity"),
                    ToStateClassID = c("Developed: Low Intensity", "Developed: Medium Intensity", "Developed: High Intensity", "Developed: Medium Intensity", "Developed: High Intensity", "Developed: High Intensity"),
                    TransitionGroupID = c("Intensification: Open to Low", "Intensification: Open to Medium", "Intensification: Open to High", 
                                          "Intensification: Low to Medium","Intensification: Low to High",
                                          "Intensification: Medium to High"))

dist_empty = tibble(Name = sort(rep(counties_df$Name,6)),
                    TransitionGroupID = rep(transtypes$TransitionGroupID,58),
                    Value=0)

# Run parallel processing of NLCD by County -------------------------------

cl = makeCluster(30)
registerDoParallel(cl)

df = foreach(i = countyIDList, .combine = "rbind", .packages = c("raster", "tidyverse")) %dopar% { 
    
    #c = counties == countyIDList[1]
    c = counties == i
    c = reclassify(c, c(-Inf,0.5,NA, 0.6,Inf,1))
    c1 = mask(nlcd01, c)
    c2 = mask(nlcd06, c)
    c3 = mask(nlcd11, c)
    c4 = mask(nlcd16, c)
    
    # Combine subset nlcd maps into dataframe
    v = tibble(
      lc01 = values(c1),
      lc06 = values(c2),
      lc11 = values(c3),
      lc16 = values(c4))
    
    # Create contigency table of amounts and probabilities and dataframe (using amounts; m variables)
    m1 = table(v[,c("lc01", "lc06")])
    p1 = as.matrix(m1 / rowSums(m1))
    d1 = m1 %>% as_tibble() %>% dplyr::select(from=lc01, to=lc06, p1=n)
    
    m2 = table(v[,c("lc06", "lc11")])
    p2 = as.matrix(m2 / rowSums(m2))
    d2 = m2 %>% as_tibble() %>% dplyr::select(from=lc06, to=lc11, p2=n)
    
    m3 = table(v[,c("lc11", "lc16")])
    p3 = as.matrix(m3 / rowSums(m3))
    d3 = m3 %>% as_tibble() %>% dplyr::select(from=lc11, to=lc16, p3=n)
    
    # Merge into single data frame
    df = left_join(d1,d2) %>% left_join(d3)
    
    # Final formatting of dataframe
    df1 = df %>% 
      filter(from %in% c(21,22,23,24), to %in% c(21,22,23,24)) %>% 
      pivot_longer(cols = c(-from, -to), names_to = "period", values_to = "area") %>%
      group_by(from, to) %>%
      summarise(sum=sum(area)*100,
                mean=mean(area)*100,
                sd=sd(area)*100,
                min=min(area)*100,
                max=max(area)*100) %>%
      ungroup() %>%
      filter(from!=to) %>%
      filter(from!=24) %>%
      ungroup() %>%
      mutate(from = as.numeric(from), to = as.numeric(to)) %>%
      mutate(mean=mean/5, sd=sd/5, min=min/5, max=max/5) %>%
      left_join(stateclass_dev, by = c("from"="ID")) %>% rename("FromStateClassID"="Name") %>%
      left_join(stateclass_dev, by = c("to"="ID")) %>% rename("ToStateClassID"="Name") %>%
      dplyr::select(FromID=from, ToID=to, FromStateClassID, ToStateClassID, sum, mean, sd, min, max) %>%
      filter(!(FromID==22 & ToID==21)) %>%
      filter(!(FromID==23 & ToID==21)) %>%
      filter(!(FromID==23 & ToID==22)) %>%
      mutate(ID=i) %>%
      left_join(counties_df) %>%
      left_join(transtypes)
    
    }


stopCluster(cl)

# Barplot to check results
ggplot(df, aes(x=Name, y=mean, fill=interaction(FromStateClassID, ToStateClassID))) +
  geom_bar(stat="identity") +
  coord_flip()




# Create Historical Distributions -----------------------------------------

# Make a copy of merged output
intensification_hist_mean = df 

# Create a dataframe which merges the processed output with the empty data so that each county and transition have a record
intensification_hist_mean_all = intensification_hist_mean %>%
  right_join(dist_empty, by=c("Name", "TransitionGroupID")) %>%
  dplyr::select(-FromID, -ToID, -FromStateClassID, -ToStateClassID, -ID) %>%
  mutate(Mean = if_else(is.na(mean), Value, mean),
         Sd = if_else(is.na(sd), Value, sd),
         Min = if_else(is.na(mean), Value, min),
         Max = if_else(is.na(mean), Value, max))


# Create Pasture to Cropland Historical Distributions datasheet
dist_intensification_hist = tibble(SecondaryStratumID = intensification_hist_mean_all$Name,
                            DistributionTypeID = intensification_hist_mean_all$TransitionGroupID,
                            ExternalVariableTypeID = "Intensification",
                            ExternalVariableMin = 2001,
                            ExternalVariableMax = 2016,
                            Value = intensification_hist_mean_all$Mean,
                            ValueDistributionTypeID = "Normal",
                            ValueDistributionFrequency = "Iteration and Timestep",
                            ValueDistributionSD = intensification_hist_mean_all$Sd,
                            ValueDistributionMin = intensification_hist_mean_all$Min,
                            ValueDistributionMax = intensification_hist_mean_all$Max)
dist_intensification_hist = dist_intensification_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_intensification_hist, "data/distributions/distribution-intensification.csv")






# Create SSP Projections --------------------------------------------------


# Combine SSP projections with historical summaries
# SSP2
intensification_ssp2 = zonal_ssp %>%
  filter(scenario=="ssp2", year>=2020) %>%
  right_join(intensification_hist_mean, by = c("County"="Name")) %>%
  dplyr::select(County, year, scenario, change_mult, TransitionGroupID, mean, sd, min, max) %>%
  mutate(MeanMult = mean*change_mult,
         SdMult = sd*change_mult,
         MinMult = min*change_mult,
         MaxMult = max*change_mult)

# Create Urbanization Historical Distributions datasheet
intensification_targets_ssp2 = tibble(SecondaryStratumID = intensification_ssp2$County,
                                      Timestep = if_else(intensification_ssp2$year==2020, 2017, intensification_ssp2$year-9),
                                      TransitionGroupID = intensification_ssp2$TransitionGroupID,
                                      Amount = intensification_ssp2$MeanMult, 
                                      DistributionType = "Normal",
                                      DistributionFrequencyID = "Iteration and Timestep",
                                      DistributionSD = intensification_ssp2$SdMult,
                                      DistributionMin = intensification_ssp2$MinMult,
                                      DistributionMax = intensification_ssp2$MaxMult)
write_csv(intensification_targets_ssp2, "data/transition-targets/transition-targets-intensification-ssp2.csv")


# SSP5
intensification_ssp5 = zonal_ssp %>%
  filter(scenario=="ssp5", year>=2020) %>%
  right_join(intensification_hist_mean, by = c("County"="Name")) %>%
  dplyr::select(County, year, scenario, change_mult, TransitionGroupID, mean, sd, min, max) %>%
  mutate(MeanMult = mean*change_mult,
         SdMult = sd*change_mult,
         MinMult = min*change_mult,
         MaxMult = max*change_mult)

# Create Urbanization Historical Distributions datasheet
intensification_targets_ssp5 = tibble(SecondaryStratumID = intensification_ssp5$County,
                                 Timestep = if_else(intensification_ssp5$year==2020, 2017, intensification_ssp5$year-9),
                                 TransitionGroupID = intensification_ssp5$TransitionGroupID,
                                 Amount = intensification_ssp5$MeanMult, 
                                 DistributionType = "Normal",
                                 DistributionFrequencyID = "Iteration and Timestep",
                                 DistributionSD = intensification_ssp5$SdMult,
                                 DistributionMin = intensification_ssp5$MinMult,
                                 DistributionMax = intensification_ssp5$MaxMult)
write_csv(intensification_targets_ssp5, "data/transition-targets/transition-targets-intensification-ssp5.csv")





















