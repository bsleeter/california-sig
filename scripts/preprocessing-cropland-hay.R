
# Header ------------------------------------------------------------------
# Code to calculate annual change rates between cropland and hay/pasture from National Land Cover Database
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions and Transition Targets based on SSP scenarios for use within the LUCAS model
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig
# NLCD data can be downloaded here: https://www.mrlc.gov/data

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")
counties_df = read_csv("data/definitions/counties.csv") %>% dplyr::select(-Description)
stateclass_df = read_csv("data/definitions/state-class-types.csv")

# Get a list of state class types for Cropland and Pasture
stateclass_haycrop = stateclass_df %>% filter(ID %in% c(81,82)) %>%
  dplyr::select(Name, ID)

# Create a list of County IDs
countyIDList = unique(counties$ic.counties)

# Create a transition type lookup table
transtypes = tibble(FromStateClassID = c("Agriculture: Pasture", "Agriculture: Cropland"),
                    ToStateClassID = c("Agriculture: Cropland", "Agriculture: Pasture"),
                    TransitionGroupID = c("Ag Change: Pasture to Cropland [Type]", "Ag Change: Cropland to Pasture [Type]"))




# Process NLCD Rasters ----------------------------------------------------

# Read in NLCD maps, project and mask to California counties 
nlcd01 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2001_Land_Cover_L48_20190424.img")
nlcd01 = projectRaster(nlcd01, counties, method = "ngb")
nlcd01 = mask(nlcd01, counties)

nlcd06 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2006_Land_Cover_L48_20190424.img")
nlcd06 = projectRaster(nlcd06, counties, method = "ngb")
nlcd06 = mask(nlcd06, counties)

nlcd11 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2011_Land_Cover_L48_20190424.img")
nlcd11 = projectRaster(nlcd11, counties, method = "ngb")
nlcd11 = mask(nlcd11, counties)

nlcd16 = raster("I:/GIS-Raster/Land Cover/NLCD/2016/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2016_Land_Cover_L48_20190424.img")
nlcd16 = projectRaster(nlcd16, counties, method = "ngb")
nlcd16 = mask(nlcd16, counties)

# Create raster stack from all layers
nlcd = stack(nlcd01, nlcd06, nlcd11, nlcd16)

# Create table from each NLCD year
v = tibble(lc01 = values(nlcd01),
           lc06 = values(nlcd06),
           lc11 = values(nlcd11),
           lc16 = values(nlcd16))






# Summarize transitions for each county -----------------------------------

# Start parallel cluster
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
    filter(from %in% c(81,82), to %in% c(81,82)) %>% 
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
    left_join(stateclass_haycrop, by = c("from"="ID")) %>% rename("FromStateClassID"="Name") %>%
    left_join(stateclass_haycrop, by = c("to"="ID")) %>% rename("ToStateClassID"="Name") %>%
    dplyr::select(FromID=from, ToID=to, FromStateClassID, ToStateClassID, sum, mean, sd, min, max) %>%
    mutate(ID=i) %>%
    left_join(counties_df) %>%
    left_join(transtypes)
  
}

# Stop parallel cluster
stopCluster(cl)


# Barplot to check results
ggplot(df, aes(x=Name, y=mean, fill=TransitionGroupID)) +
  geom_bar(stat="identity") +
  coord_flip()







# Create historical distributions for Pasture to Cropland -----------------------------------------

# Make a copy of merged output
pastcrop_hist_mean = df %>% filter(FromStateClassID=="Agriculture: Pasture")

# Create Pasture to Cropland Historical Distributions datasheet
dist_pastcrop_hist = tibble(SecondaryStratumID = pastcrop_hist_mean$Name,
                       DistributionTypeID = "Ag Change: Pasture to Cropland",
                       ExternalVariableTypeID = "Ag Change",
                       ExternalVariableMin = 2001,
                       ExternalVariableMax = 2016,
                       Value = pastcrop_hist_mean$mean,
                       ValueDistributionTypeID = "Normal",
                       ValueDistributionFrequency = "Iteration and Timestep",
                       ValueDistributionSD = pastcrop_hist_mean$sd)
dist_pastcrop_hist = dist_pastcrop_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_pastcrop_hist, "data/distributions/distribution-ag-change-pasture-cropland.csv")






# Create historical distributions for Cropland to Pasture -----------------------------------------

# Make a copy of merged output
croppast_hist_mean = df %>% filter(FromStateClassID=="Agriculture: Cropland")

# Create Pasture to Cropland Historical Distributions datasheet
dist_croppast_hist = tibble(SecondaryStratumID = croppast_hist_mean$Name,
                            DistributionTypeID = "Ag Change: Cropland to Pasture",
                            ExternalVariableTypeID = "Ag Change",
                            ExternalVariableMin = 2001,
                            ExternalVariableMax = 2016,
                            Value = croppast_hist_mean$mean,
                            ValueDistributionTypeID = "Normal",
                            ValueDistributionFrequency = "Iteration and Timestep",
                            ValueDistributionSD = croppast_hist_mean$sd)
dist_croppast_hist = dist_croppast_hist %>% arrange(SecondaryStratumID, ExternalVariableMin)
write_csv(dist_croppast_hist, "data/distributions/distribution-ag-change-cropland-pasture.csv")
