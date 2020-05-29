
# Header ------------------------------------------------------------------
# Code to calculate urban change rates for each type of developed class from the National Land Cover Database
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces Historical Distributions of urban change types
# NLCD data can be downloaded here: https://www.mrlc.gov/data
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------


library(raster)
library(sf)
library(tidyverse)

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")

# Read in Counties dataframe
counties_df = read_csv("data/definitions/counties.csv") %>% dplyr::select(-Description)

# Read in list of State Class Types
stateclass_df = read_csv("data/definitions/state-class-types.csv")

# Get a list of state class types for Developed
stateclass_dev = stateclass_df %>% dplyr::select(Name, ID)

# Create a list of County IDs
countyIDList = unique(counties$ic.counties)

# Create a transition type lookup table
transtypes = tibble(ToStateClassID = c("Developed: Open Space", "Developed: Low Intensity", "Developed: Medium Intensity", "Developed: High Intensity"),
                    TransitionGroupID = c("Urbanization: Open [Type]", "Urbanization: Low [Type]", "Urbanization: Medium [Type]", "Urbanization: High [Type]"))






# Read in NLCD and project and mask to California -------------------------

# Read in NLCD maps
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

nlcd = stack(nlcd01, nlcd06, nlcd11, nlcd16)







# Create contigency tables ------------------------------------------------

# Create a table from the 4 raster layers
v = tibble(
  lc01 = values(nlcd01),
  lc06 = values(nlcd06),
  lc11 = values(nlcd11),
  lc16 = values(nlcd16))

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
merged_tables = left_join(d1,d2) %>% left_join(d3)






# Create Dataframe of Developed Change Types --------------------------------------------------

# Formatting of dataframe
urban_change_type = merged_tables %>% 
  filter(!from %in% c(21,22,23,24), to %in% c(21,22,23,24)) %>% 
  pivot_longer(cols = c(-from, -to), names_to = "period", values_to = "area") %>%
  group_by(to) %>%
  summarise(sum=sum(area, na.rm=T)*100, sd=sd(area, na.rm=T)*100) %>%
  mutate(pct=sum/sum(sum), pctsd=sd/sum) %>%
  ungroup() %>%
  mutate(to = as.numeric(to)) %>%
  left_join(stateclass_dev, by = c("to"="ID")) %>% rename("ToStateClassID"="Name") %>%
  dplyr::select(ToID=to, ToStateClassID, sum, sd, pct, pctsd) %>%
  left_join(transtypes)


# Barplot to check results
ggplot(urban_change_type, aes(x=TransitionGroupID, y=pct)) +
  geom_bar(stat="identity") +
  coord_flip()

# Make a copy of merged output
urban_mult = tibble(TransitionGroupID = df1$TransitionGroupID,
                    Amount = df1$pct,
                    DistributionType = "Normal",
                    DistributionFrequencyID = "Iteration and Timestep",
                    DistributionSD = df1$pctsd)
write_csv(urban_mult, "data/transition-multipliers/transition-multipliers-urbanization-types.csv")



