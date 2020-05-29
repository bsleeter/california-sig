

# Header ------------------------------------------------------------------
# Code to preprocess U.S. EPA's ICLUS Land Use Scenario data
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces county summaries of ICLUS change in developed land use
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# ICLUS v2 data were downloaded from https://iclus.epa.gov/
# ICLUS v2 data used in this analysis utilized historical climate.

# ICLUS SSP county summaries available at https://github.com/bsleeter/california-sig/tree/master/docs/ssp

# Last Modified 2020-05-28





# Setup -------------------------------------------------------------------

# Read in counties table
counties_tbl = read_csv("data/definitions/counties.csv")

# Read in California Counties raster
counties = raster("data/initial-conditions/ic-counties.tif")


# Read ICLUS SSP2 raster data ----------------------------------------------------

ssp2_list = list.files(path = "I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc", pattern = "*.tif$") # List rasters
ssp2_hist = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc/", ssp2_list[1:2])) # Create raster stack of historical data
ssp2_proj = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp2_nocc/ICLUS_v2.1_landuse_conus_ssp2_nocc/", ssp2_list[3:11])) # Create raster stock of projected data
ssp2_hist = projectRaster(ssp2_hist, counties, method="ngb") # Project to California counties raster
ssp2_hist = mask(ssp2_hist, counties) # Mask to California extent
ssp2_proj = projectRaster(ssp2_proj, counties, method="ngb") # Project to California counties raster
ssp2_proj = mask(ssp2_proj, counties) # Mask to California extent
ssp2 = stack(ssp2_hist, ssp2_proj) # Stack historical and projected rasters
ssp2 = reclassify(ssp2, c(-Inf,11.5,0, 11.6,16.5,1, 16.6,17.5,0, 17.6,Inf,0)) # Reclassify stack to include only developed classes (12-16; Exurban High, Suburban, Urban Low, Urban High, Commercial)




# Read ICLUS SSP5 raster data ---------------------------------------------

ssp5_list = list.files(path = "I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc", pattern = "*.tif$") # List rasters
ssp5_hist = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc/", ssp5_list[1:2])) # Create raster stack of historical data
ssp5_proj = stack(paste0("I:/GIS-Raster/iCLUS/ICLUS V2/ICLUS_v2.1_land_use_conus_ssp5_nocc/ICLUS_v2.1_landuse_conus_ssp5_nocc/", ssp5_list[3:11])) # Create raster stock of projected data
ssp5_hist = projectRaster(ssp5_hist, counties, method="ngb") # Project to California counties raster
ssp5_hist = mask(ssp5_hist, counties) # Mask to California extent
ssp5_proj = projectRaster(ssp5_proj, counties, method="ngb") # Project to California counties raster
ssp5_proj = mask(ssp5_proj, counties) # Mask to California extent
ssp5 = stack(ssp5_hist, ssp5_proj) # Stack historical and projected rasters
ssp5 = reclassify(ssp5, c(-Inf,11.5,0, 11.6,16.5,1, 16.6,17.5,0, 17.6,Inf,0)) # Reclassify stack to include only developed classes (12-16; Exurban High, Suburban, Urban Low, Urban High, Commercial)






# Zonal Summaries ---------------------------------------------------------

# SSP2 Zonal summary for each county
zonal_ssp2 = data.frame(zonal(ssp2, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "year", values_to = "area") %>%
  mutate(year = str_replace(year, pattern = "_ssp2_nocc", replacement = "")) %>%
  mutate(year = str_remove(year, pattern = "ICLUS_v2.1_land_use_conus_")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(scenario = "ssp2")

# SSP5 Zonal summary for each county
zonal_ssp5 = data.frame(zonal(ssp5, counties, "sum")) %>%
  pivot_longer(-zone, names_to = "year", values_to = "area") %>%
  mutate(year = str_replace(year, pattern = "_ssp5_nocc", replacement = "")) %>%
  mutate(year = str_remove(year, pattern = "ICLUS_v2.1_land_use_conus_")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(scenario = "ssp5")





# Calculate the annual change by county for the 2000-2010 period (reference period)
zonal_ssp_ref = bind_rows(zonal_ssp2, zonal_ssp5) %>%
  group_by(zone, scenario) %>%
  arrange(year) %>%
  mutate(ref_annual_change = (area-lag(area))/10) %>%
  mutate(ref_annual_change = if_else(ref_annual_change<=0, 0.01, ref_annual_change)) %>%
  filter(year==2010) %>%
  ungroup() %>%
  dplyr::select(zone, ref_annual_change)

# Calculate the difference in county change rates relative to the reference period (2000-2010)
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

# Write county change multipliers to disk
write_csv(zonal_ssp, "docs/ssp/iclus-ssp-zonal-county-summary.csv")