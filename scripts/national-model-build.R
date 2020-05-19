

library(raster)
library(tidyverse)
library(rsyncrosim)





# ssimDir = "F:/national-assessment/data/ssim-libs/"
ssimDir = "F:/national-assessment/models/"
SyncroSimDir <- "C:/Program Files/SyncroSim/"
mySession <- session(SyncroSimDir)

# myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model Conus.ssim"), session = mySession)
# myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model.ssim"), session = mySession)
myLibrary = ssimLibrary(name = paste0(ssimDir,"National Assessment Model.ssim"), session = mySession)
myProject = project(myLibrary, project="LUCAS-National")



########################
# Definitions (ST-Sim) #
########################

##### ST-Sim Terminology #####
sheetName <- "stsim_Terminology"
mySheet <- datasheet(myProject, name=sheetName)
mySheet$AmountLabel[1] <- "Area"
mySheet$AmountUnits[1] <- "Hectares"
mySheet$StateLabelX[1] <- "LULC"
mySheet$StateLabelY[1] <- "Subclass"
mySheet$PrimaryStratumLabel[1] <- "Ecological Boundary"
mySheet$SecondaryStratumLabel[1] <- "Administrative Boundary"
mySheet$SecondaryStratumLabel[1] <- "Ownership"
mySheet$TimestepUnits[1] <- "Year"
saveDatasheet(myProject, mySheet, sheetName)

##### ST-Sim Primary Strata #####
sheetName <- "stsim_Stratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/ecoregions.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Secondary Strata ##### 
sheetName <- "stsim_SecondaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/states.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Tertiary Strata ##### 
sheetName <- "stsim_TertiaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/land-managers-strata.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim State Label x #####
sheetName <- "stsim_StateLabelX"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/state-label-xid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim State Label y #####
sheetName <- "stsim_StateLabelY"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/state-label-yid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim State Class #####
sheetName <- "stsim_StateClass"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/state-class-types.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Transition Types #####
sheetName <- "stsim_TransitionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/transition-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Transition Groups #####
sheetName <- "stsim_TransitionGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/transition-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim Transition Types by Group #####
sheetName <- "stsim_TransitionTypeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/transition-types-by-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim Transition Simulation Groups #####
# Only used for projection models
sheetName <- "stsim_TransitionSimulationGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/transition-simulation-groups.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim Age Type #####
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- 300
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Age Group #####
maxAge = 300
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Attribute Groups #####
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/state-attribute-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim State Attribute Types #####
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/state-attribute-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### ST-Sim Distributions #####
sheetName <- "corestime_DistributionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/distributions.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### ST-Sim External Variables #####
sheetName <- "corestime_ExternalVariableType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/external-variables.csv")
saveDatasheet(myProject, mySheet, name=sheetName)



##### SF Stock Types #####
sheetName <- "stsimsf_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/stock-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### SF Stock Groups #####
sheetName <- "stsimsf_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/stock-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### SF Flow Types #####
sheetName <- "stsimsf_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/flow-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

##### SF Flow Groups #####
sheetName <- "stsimsf_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv("F:/national-assessment/data/definitions/flow-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)


















####################################
# Sub-scenario datasheets (ST-Sim) #
####################################

##### ST-Sim Run Control #####
maxTimestep <- 2014
maxIteration <- 1
minTimestep <- 2001
minIteration <- 1
myScenario <- scenario(myProject, scenario <- paste0("Run Control [Spatial; 2001-2014; 1 MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

##### Pathway Diagrams #####

# Urbanization
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Urbanization]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-urbanization.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Change
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Change]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-agchange.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Expansion
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Expansion]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-agexpansion.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Contraction
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Contraction]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-agcontraction.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Urban Intensification
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Intensification]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-intensification.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Fire
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Fire]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-fire.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Drought
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Insect]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-insect.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Harvest]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/definitions/pathway-diagram-probabilistic-harvest.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Pathway Dependencies
myScenario <- scenario(myProject, scenario = "Pathway Diagram")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("Pathway Diagram [Urbanization]", "Pathway Diagram [Ag Change]", "Pathway Diagram [Ag Expansion]", "Pathway Diagram [Ag Contraction]", "Pathway Diagram [Intensification]",
                         "Pathway Diagram [Fire]", "Pathway Diagram [Insect]", "Pathway Diagram [Harvest]"))



##### Distributions #####

myScenario <- scenario(myProject, scenario = "Distirbutions [Urbanization]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-urbanization.csv")  %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-intensification.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Change]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-agchange.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-agexpansion.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-agcontraction.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-fire.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-insect.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/distributions/distribution-harvest.csv") %>% rename("Value" = "Amount")
saveDatasheet(myScenario, mySheet, sheetName)


myScenario <- scenario(myProject, scenario = "Distributions")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Urbanization]", "Distirbutions [Intensification]", "Distirbutions [Ag Change]",
                         "Distirbutions [Ag Expansion]", "Distirbutions [Ag Contraction]", "Distirbutions [Fire]", "Distirbutions [Insect]", "Distirbutions [Harvest]"))


##### External Variables #####

myScenario <- scenario(myProject, scenario = "External Variables")
sheetName <- "corestime_ExternalVariableValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/external-variables/external-variables-bau.csv")
saveDatasheet(myScenario, mySheet, sheetName)


datasheet(myScenario)



##### ST-Sim Initial conditions #####

# California Model
#myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial]")
#sheetName <- "stsim_InitialConditionsSpatial"
#mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
#mySheet[1, "StratumFileName"] <- "E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_Ecoregions_1km.tif"
#mySheet[1, "SecondaryStratumFileName"] <- "E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_Counties_1km.tif"
#mySheet[1, "TertiaryStratumFileName"] <- "E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_Ownership_1km.tif"
#mySheet[1, "StateClassFileName"] <- "E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_StateClass_ForestGroups_1km.tif"
#mySheet[1, "AgeFileName"] <- "E:/california-carbon-futures/Build/model_base/ccf_v5/R Inputs/Data/initial-conditions/IC_Age_1km.tif"
#saveDatasheet(myScenario, mySheet, sheetName)

# Imputed Age
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-ecoregion.tif"
mySheet[1, "SecondaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-states.tif"
mySheet[1, "TertiaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-land-managers.tif"
mySheet[1, "StateClassFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-state-class.tif"
mySheet[1, "AgeFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-imputed-age.tif"
saveDatasheet(myScenario, mySheet, sheetName)

# Mapped Age
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial; Mapped Age]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-ecoregion.tif"
mySheet[1, "SecondaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-states.tif"
mySheet[1, "TertiaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-land-managers.tif"
mySheet[1, "StateClassFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-state-class.tif"
mySheet[1, "AgeFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-age.tif"
saveDatasheet(myScenario, mySheet, sheetName)

##### ST-Sim Output options #####
myScenario <- scenario(myProject, scenario = "Output Options [Spatial]")
sheetName <- "stsim_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "SummaryOutputSC"] <- T
mySheet[1, "SummaryOutputSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSCZeroValues"] <- F
mySheet[1, "SummaryOutputTR"] <- T
mySheet[1, "SummaryOutputTRTimesteps"] <- 1
mySheet[1, "SummaryOutputTRIntervalMean"] <- F
mySheet[1, "SummaryOutputTRSC"] <- T
mySheet[1, "SummaryOutputTRSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSA"] <- T
mySheet[1, "SummaryOutputSATimesteps"] <- 1
mySheet[1, "SummaryOutputTA"] <- T
mySheet[1, "SummaryOutputTATimesteps"] <- 1
mySheet[1, "SummaryOutputOmitSS"] <- F
mySheet[1, "SummaryOutputOmitTS"] <- F
mySheet[1, "RasterOutputSC"] <- T
mySheet[1, "RasterOutputSCTimesteps"] <- 1
mySheet[1, "RasterOutputTR"] <- T
mySheet[1, "RasterOutputTRTimesteps"] <- 1
mySheet[1, "RasterOutputAATP"] <- T
mySheet[1, "RasterOutputAATPTimesteps"] <- 15
mySheet[1, "RasterOutputTransitionEvents"] <- T
mySheet[1, "RasterOutputTransitionEventTimesteps"] <- 1
mySheet[1, "RasterOutputAge"] <- T
mySheet[1, "RasterOutputAgeTimesteps"] <- 1
saveDatasheet(myScenario, mySheet, sheetName)





##### Adjacency Settings/Multipliers #####

myScenario = scenario(myProject, scenario = "Adjacency Settings")
sheetName = "stsim_TransitionAdjacencySetting"
mySheet = read.csv("F:/national-assessment/data/adjacency/transition-adjacency-setting.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName = "stsim_TransitionAdjacencyMultiplier"
mySheet = datasheet(myScenario, sheetName, optional = T, empty = T)
mySheet = read.csv("F:/national-assessment/data/adjacency/transition-adjacency-multipliers.csv")
saveDatasheet(myScenario, mySheet, sheetName)

##### ST-Sim Transition Targets #####

myScenario <- scenario(myProject, scenario = "Transition Targets [Reference]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv("F:/national-assessment/data/transition-targets/transition-targets-reference.csv")
saveDatasheet(myScenario, mySheet, sheetName)

##### ST-Sim Spatial Multipliers #####

# Urbanization
dir1 = "F:/national-assessment/data/spatial-multipliers/urbanization-high/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Urbanization: High [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/urbanization-medium/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Urbanization: Medium [Type]"

dir3 = "F:/national-assessment/data/spatial-multipliers/urbanization-low/"
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Urbanization: Low [Type]"

dir4 = "F:/national-assessment/data/spatial-multipliers/urbanization-open/"
list4 = list.files(dir4, pattern = "*.tif$")
tg4 = "Urbanization: Open [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Urbanization]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg4, MultiplierFileName = paste0(dir4,list4)))
saveDatasheet(myScenario, mySheet, sheetName)

# Intensification
dir1 = "F:/national-assessment/data/spatial-multipliers/intensification-open-low/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Intensification: Open to Low [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/intensification-open-medium/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Intensification: Open to Medium [Type]"

dir3 = "F:/national-assessment/data/spatial-multipliers/intensification-open-high/"
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Intensification: Open to High [Type]"

dir4 = "F:/national-assessment/data/spatial-multipliers/intensification-low-medium/"
list4 = list.files(dir4, pattern = "*.tif$")
tg4 = "Intensification: Low to Medium [Type]"

dir5 = "F:/national-assessment/data/spatial-multipliers/intensification-low-high/"
list5 = list.files(dir5, pattern = "*.tif$")
tg5 = "Intensification: Low to High [Type]"

dir6 = "F:/national-assessment/data/spatial-multipliers/intensification-medium-high/"
list6 = list.files(dir6, pattern = "*.tif$")
tg6 = "Intensification: Medium to High [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg4, MultiplierFileName = paste0(dir4,list4)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg5, MultiplierFileName = paste0(dir5,list5)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg6, MultiplierFileName = paste0(dir6,list6)))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Change
dir1 = "F:/national-assessment/data/spatial-multipliers/ag-change-cropland-pasture/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Ag Change: Cropland to Pasture [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/ag-change-pasture-cropland/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Ag Change: Pasture to Cropland [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Change]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Contraction
dir1 = "F:/national-assessment/data/spatial-multipliers/ag-contraction-cropland/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Ag Contraction: Cropland"

dir2 = "F:/national-assessment/data/spatial-multipliers/ag-contraction-pasture/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Ag Contraction: Pasture"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Expansion
dir1 = "F:/national-assessment/data/spatial-multipliers/ag-expansion-cropland/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Ag Expansion: Cropland [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/ag-expansion-pasture/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Ag Expansion: Pasture [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2012,5), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
saveDatasheet(myScenario, mySheet, sheetName)

# Fire
dir1 = "F:/national-assessment/data/spatial-multipliers/fire-high-severity/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Fire: High Severity [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/fire-medium-severity/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Fire: Medium Severity [Type]"

dir3 = "F:/national-assessment/data/spatial-multipliers/fire-low-severity/"
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Fire: Low Severity [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2014,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2014,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2014,1), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
saveDatasheet(myScenario, mySheet, sheetName)

# Insect
dir1 = "F:/national-assessment/data/spatial-multipliers/insect-high-severity/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Insect: High Severity [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/insect-medium-severity/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Insect: Medium Severity [Type]"

dir3 = "F:/national-assessment/data/spatial-multipliers/insect-low-severity/"
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Insect: Low Severity [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2015,1), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest
dir1 = "F:/national-assessment/data/spatial-multipliers/clearcut/"
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Management: Forest Clearcut [Type]"

dir2 = "F:/national-assessment/data/spatial-multipliers/selection/"
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Management: Forest Selection [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Forest Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2014,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(2002,2014,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Spatial Multipliers

myScenario <- scenario(myProject, scenario = "Spatial Multipliers")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Spatial Multipliers [Urbanization]", "Spatial Multipliers [Intensification]", "Spatial Multipliers [Ag Change]",
                         "Spatial Multipliers [Ag Contraction]", "Spatial Multipliers [Ag Expansion]", "Spatial Multipliers [Fire]",
                         "Spatial Multipliers [Insect]", "Spatial Multipliers [Forest Harvest]"))


datasheet(myScenario)






##### ST-Sim State Attribute Values #####

# Adjacency Attributes
myScenario <- scenario(myProject, scenario = "State Attributes [Adjacency]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/state-attributes/state-attribute-values-adjacency.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# Fire as last disturbance
myScenario <- scenario(myProject, scenario = "State Attributes [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/state-attributes/state-attribute-values-fire.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest as last disturbance
myScenario <- scenario(myProject, scenario = "State Attributes [Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/state-attributes/state-attribute-values-harvest.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# Merge State Attributes
myScenario <- scenario(myProject, scenario = "State Attributes with Fire")
mergeDependencies(myScenario) = T
dependency(myScenario, c("State Attributes [Adjacency]", "State Attributes [Fire]"))

myScenario <- scenario(myProject, scenario = "State Attributes with Harvest")
mergeDependencies(myScenario) = T
dependency(myScenario, c("State Attributes [Adjacency]", "State Attributes [Harvest]"))


















####################################
# Sub-scenario datasheets (Stock-Flows)
####################################

##### SF Initial Stocks #####
# Imputed Age
myScenario <- scenario(myProject, scenario = "SF Initial Stocks [Spatial]")
sheetName <- "stsimsf_InitialStockSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Fine Root", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/Biomass Fine Root [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Coarse Root", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/Biomass Coarse Root [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Foliage", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/Biomass Foliage [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Merchantable", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/Biomass Merchantable [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Other Wood", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/Biomass Other Wood [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Very Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Aboveground Very Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Very Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Belowground Very Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Aboveground Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Belowground Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Medium", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Aboveground Medium [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Slow", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Aboveground Slow [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Slow", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Belowground Slow [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Branch", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Snag Branch [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Stem", RasterFileName = "F:/national-assessment/data/initial-stocks/imputed/DOM Snag Stem [Type].tif"))
saveDatasheet(myScenario, mySheet, sheetName)

# Mapped Age
myScenario <- scenario(myProject, scenario = "SF Initial Stocks [Spatial; Mapped Age]")
sheetName <- "stsimsf_InitialStockSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Fine Root", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/Biomass Fine Root [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Coarse Root", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/Biomass Coarse Root [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Foliage", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/Biomass Foliage [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Merchantable", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/Biomass Merchantable [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "Biomass: Other Wood", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/Biomass Other Wood [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Very Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Aboveground Very Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Very Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Belowground Very Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Aboveground Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Fast", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Belowground Fast [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Medium", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Aboveground Medium [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Aboveground Slow", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Aboveground Slow [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Belowground Slow", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Belowground Slow [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Branch", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Snag Branch [Type].tif"))
mySheet = addRow(mySheet, data.frame(StockTypeID = "DOM: Snag Stem", RasterFileName = "F:/national-assessment/data/initial-stocks/mapped/DOM Snag Stem [Type].tif"))
saveDatasheet(myScenario, mySheet, sheetName)

##### SF Flow Pathways Diagram #####

# Base FLows
myScenario <- scenario(myProject, scenario = "Flow Pathways [Base Flows]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-pathway-diagram.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-pathways.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Transition-triggered flows
myScenario <- scenario(myProject, scenario = "Flow Pathways [Transition Flows]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-pathway-diagram.csv")
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-pathways-lulc-disturbance.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Flow Pathways Sub-Scenarios
myScenario = scenario(myProject, scenario = "Flow Pathways")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("Flow Pathways [Base Flows]", "Flow Pathways [Transition Flows]"))

##### SF Stock Group Membership #####
myScenario <- scenario(myProject, scenario = "SF Stock Group Membership")
sheetName <- "stsimsf_StockTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/stock-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

##### SF Flow Group Membership #####
myScenario <- scenario(myProject, scenario = "SF Flow Group Membership")
sheetName <- "stsimsf_FlowTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

##### SF Flow Flow Order #####
myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrder"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("F:/national-assessment/data/stock-flow-model/flow-order.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrderOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"ApplyEquallyRankedSimultaneously"] = T
saveDatasheet(myScenario, mySheet, sheetName)

##### SF Output Options #####
myScenario <- scenario(myProject, scenario = "SF Output Options")
sheetName <- "stsimsf_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"SummaryOutputST"] = T
mySheet[1,"SummaryOutputFL"] = T
mySheet[1,"SummaryOutputSTTimesteps"] = 1
mySheet[1,"SummaryOutputFLTimesteps"] = 1
mySheet[1,"SpatialOutputST"] = T
mySheet[1,"SpatialOutputFL"] = T
mySheet[1,"SpatialOutputSTTimesteps"] = 1
mySheet[1,"SpatialOutputFLTimesteps"] = 1
saveDatasheet(myScenario, mySheet, sheetName)


















##### SF Flow Spatial Multipliers #####
indir = "F:/national-assessment/data/flow-spatial-multipliers/growth/"
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Growth]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/growth/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2017), FlowGroupID = "Net Growth: Total", MultiplierFileName = paste(indir,gcm,"/",rcp,list, sep="")[1:16])
saveDatasheet(myScenario, mySheet, sheetName)

indir = "F:/national-assessment/data/flow-spatial-multipliers/q10Fast/"
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Fast]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/q10Fast/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2014), FlowGroupID = "Q10 Fast Flows", MultiplierFileName = paste(indir,gcm,"/",rcp,list, sep="")[1:13])
saveDatasheet(myScenario, mySheet, sheetName)

indir = "F:/national-assessment/data/flow-spatial-multipliers/q10Slow/"
gcm = "historical"
rcp = ""

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Slow]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/q10Slow/historical/", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2014), FlowGroupID = "Q10 Slow Flows", MultiplierFileName = paste(indir,gcm,"/",rcp,list, sep="")[1:13])
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "SF Flow Spatial Multipliers")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("SF Flow Spatial Multipliers [Growth]", "SF Flow Spatial Multipliers [Q10 Fast]", "SF Flow Spatial Multipliers [Q10 Slow]"))



##### SF Flow Multipliers #####
myScenario <- scenario(myProject, scenario = "SF Flow Multipliers")
sheetName <- "stsimsf_FlowMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Net Growth: Total", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Fast Flows", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Slow Flows", Value = 0.01))
saveDatasheet(myScenario, mySheet, sheetName)


##### Create a multi-processing mask file #####

eco = raster("F:/national-assessment/data/initial-conditions/ic-ecoregion.tif")
r1 = reclassify(eco, c(9.5,76.6,NA, 78.5,84.6,NA))
r1 = reclassify(r1, c(0,85,1))

r2 = reclassify(eco, c(0,11.6,NA, 14.5,79.6,NA, 80.5,85.6,NA))
r2 = reclassify(r2, c(0,85,2))

r3 = reclassify(eco, c(-Inf,9.5,NA, 11.5,14.5,NA, 17.5,40.5,NA, 41.5,85.5,NA))
r3 = reclassify(r3, c(0,85,3))

r4 = reclassify(eco, c(-Inf,17.5,NA, 23.5,78.5,NA, 79.5,80.5,NA, 81.5,85.5,NA))
r4 = reclassify(r4, c(0,85,4))

r5 = reclassify(eco, c(-Inf,41,NA, 43,45,NA, 46.5,85,NA))
r5 = reclassify(r5, c(0,85,5))

r6 = reclassify(eco, c(-Inf,46.5,NA, 57.5,85,NA))
r6 = reclassify(r6, c(0,85,6))

r7 = reclassify(eco, c(-Inf,23.5,NA, 30.5,43.5,NA, 44.5,85.5,NA))
r7 = reclassify(r7, c(0,85,7))

r8 = reclassify(eco, c(-Inf,30.5,NA, 40.5,71.5,NA, 74.5,85.5,NA))
r8 = reclassify(r8, c(0,85,8))

r9 = reclassify(eco, c(-Inf,57.5,NA, 62.5,63.5,NA, 64.5,65.5,NA, 71.5,81.5,NA, 84.5,85.5,NA))
r9 = reclassify(r9, c(0,85,9))

r10 = reclassify(eco, c(-Inf,44.5,NA, 45.5,62.5,NA, 63.5,64.5,NA, 65.5,74.5,NA, 76.5,85.5,NA))
r10 = reclassify(r10, c(0,85,10))

plot(r10)

mpRegions = mean(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10, na.rm=TRUE)
plot(mpRegions)
writeRaster(mpRegions, "F:/national-assessment/data/spatial-multiprocessing/multi-processing-regions.tif", overwrite=T, format="GTiff")

writeRaster(r1, "F:/national-assessment/data/spatial-multiprocessing/r1-pacific-coast.tif", overwrite=T, format="GTiff")
writeRaster(r2, "F:/national-assessment/data/spatial-multiprocessing/r2-basin-range.tif", overwrite=T, format="GTiff")
writeRaster(r3, "F:/national-assessment/data/spatial-multiprocessing/r3-northern-rockies.tif", overwrite=T, format="GTiff")
writeRaster(r4, "F:/national-assessment/data/spatial-multiprocessing/r4-interior-plateau.tif", overwrite=T, format="GTiff")
writeRaster(r5, "F:/national-assessment/data/spatial-multiprocessing/r5-northern-plains.tif", overwrite=T, format="GTiff")
writeRaster(r6, "F:/national-assessment/data/spatial-multiprocessing/r6-great-lakes.tif", overwrite=T, format="GTiff")
writeRaster(r7, "F:/national-assessment/data/spatial-multiprocessing/r7-great-plains.tif", overwrite=T, format="GTiff")
writeRaster(r8, "F:/national-assessment/data/spatial-multiprocessing/r8-eastern-plains.tif", overwrite=T, format="GTiff")
writeRaster(r9, "F:/national-assessment/data/spatial-multiprocessing/r9-north-east.tif", overwrite=T, format="GTiff")
writeRaster(r10, "F:/national-assessment/data/spatial-multiprocessing/r10-south-east.tif", overwrite=T, format="GTiff")

testRegions = mean(r1,r3, na.rm=TRUE)
plot(testRegions)
writeRaster(testRegions, "F:/national-assessment/data/spatial-multiprocessing/multi-processing-regions-test.tif", overwrite=T, format="GTiff")

