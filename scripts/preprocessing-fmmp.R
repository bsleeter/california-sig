
library(tidyverse)
library(readxl)
library(doParallel)
library(foreach)

cnames = c("From_Class", "to", "P_Farmland","SI_Farmland", "U_Farmland", "LI_Farmland", "Total_Farmland", "Grazing", "Total_Agriculture", "Urban", "Other", "Water", "Total")
luClasses = c("P_Farmland","SI_Farmland", "U_Farmland", "LI_Farmland", "Total_Farmland", "Grazing", "Total_Agriculture", "Urban", "Other", "Water", "Total")

files = list.files("docs/fmmp/conversion-tables")
fileList = paste0("docs/fmmp/conversion-tables/", files)

cl = makeCluster(20)
registerDoParallel(cl)

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
stopCluster(cl)

# Conver to long format
mergedData_Long = mergedData %>%
  pivot_longer(cols = c(-From_Class, -County, -Year), names_to = "To_Class", values_to = "Acres") %>%
  dplyr::select(Year, County, From_Class, To_Class, Acres) %>%
  mutate(County = str_remove(County, " - Important Farmland Area")) %>%
  separate(Year, into = c("FromYear", "ToYear"), sep = "-") %>%
  mutate(Hectares = (Acres*0.404686)/2)
write_csv(mergedData_Long, "docs/fmmp/fmmp-conversion-totals.csv")

















