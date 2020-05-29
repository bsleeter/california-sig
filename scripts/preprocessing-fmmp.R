

# Header ------------------------------------------------------------------
# Code to preprocess California'a Farmland Mapping and Monitoring Program data (FMMP)
# Contact: Benjamin M. Sleeter, U.S. Geological Survey; bsleeter@usgs.gov

# Script produces county summaries of FMMP data used by other scripts to produce historical and projected model inputs
# All model code can be found within GitHub Repository https://github.com/bsleeter/california-sig

# FMMP data downloaded on 2020-05-22 from https://www.conservation.ca.gov/dlrp/fmmp/Pages/county_info.aspx
# FMMP county summaries available at https://github.com/bsleeter/california-sig/tree/master/docs/fmmp/conversion-tables

# Last Modified 2020-05-28



# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(doParallel)
library(foreach)


# Define list of column names
cnames = c("From_Class", "to", "P_Farmland","SI_Farmland", "U_Farmland", "LI_Farmland", "Total_Farmland", "Grazing", "Total_Agriculture", "Urban", "Other", "Water", "Total")

# Define list of row names in forst column
luClasses = c("P_Farmland","SI_Farmland", "U_Farmland", "LI_Farmland", "Total_Farmland", "Grazing", "Total_Agriculture", "Urban", "Other", "Water", "Total")

# Create a list of FMMP files
files = list.files("docs/fmmp/conversion-tables")
fileList = paste0("docs/fmmp/conversion-tables/", files)




# Read and format FMMP raw data using parallel processing -----------------

cl = makeCluster(20) # Start parallel cluster
registerDoParallel(cl)

# Read in each FMMP file and process...
mergedData = foreach(i = fileList, .packages=c("tidyverse", "readxl"), .combine=rbind) %dopar% {
  
  #i = fileList[2]
  year = read_excel(i, range = "A1:A3", col_names = "data")
  df = read_excel(i, range = "A28:M38", col_names = cnames) %>%
    dplyr::select(-to) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(From_Class = luClasses, County = year$data[2], Year = year$data[3]) %>%
    mutate(County = str_remove(County, pattern = " COUNTY"),
           Year = str_remove(Year, " Land Use Conversion")) %>%
    mutate(County = str_to_title(County))
}

stopCluster(cl) # Stop parallel cluster




# Convert data to long format and write to disk ---------------------------

mergedData_Long = mergedData %>%
  pivot_longer(cols = c(-From_Class, -County, -Year), names_to = "To_Class", values_to = "Acres") %>%
  dplyr::select(Year, County, From_Class, To_Class, Acres) %>%
  mutate(County = str_remove(County, " - Important Farmland Area")) %>%
  separate(Year, into = c("FromYear", "ToYear"), sep = "-") %>%
  mutate(Hectares = (Acres*0.404686)/2)
write_csv(mergedData_Long, "docs/fmmp/fmmp-conversion-totals.csv")

















