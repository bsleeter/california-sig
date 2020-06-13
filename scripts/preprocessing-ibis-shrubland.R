

library(tidyverse)

# IBIS Analysis and Output ------------------------------------------------

# Biomass Growth Parameters
# Biomass Turnover Parameters
# NPP

# Data taken from IBIS Inputs
branch_fraction = 0.2
root_fraction = 0.7
leaf_fraction = 0.1





# Merge IBIS Biomass with LUCAS DOM Flows -------------------------------



# Read in the forest flow pathways

forest_flows = read_csv("F:/national-assessment/data/stock-flow-model/flow-pathways.csv")

forest_dom_flows = forest_flows %>%
  filter(FromStockTypeID == "DOM: Aboveground Fast", FromStateClassID == "Forest: Pinyon/Juniper Group") %>%
  mutate(FromStateClassID = "Shrubland: Big Sagebrush")
