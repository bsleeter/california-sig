

library(raster)
library(tidyverse)
library(rsyncrosim)





# ssimDir = "F:/national-assessment/data/ssim-libs/"
ssimDir = "models/testing/"
SyncroSimDir <- "C:/Program Files/SyncroSim/"
mySession <- session(SyncroSimDir)

# myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model Conus.ssim"), session = mySession)
# myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model.ssim"), session = mySession)
myLibrary = ssimLibrary(name = paste0(ssimDir,"california-sig.ssim"), session = mySession, addon = "stsimsf")
myProject = project(myLibrary, project="Definitions")
dataDir = "data/"
prefixDir = "D:/california-sig/"
libpath = paste0(prefixDir, ssimDir, "california-sig.ssim")


# Set up First Level Folder Structure -------------------------------------------------

#create child folder for Working Scenarios
args = list(create=NULL, folder=NULL, lib=libpath, name="working-scenarios", tpid=projectId(myProject))
ret = command(args, session=mySession)
working_scn_fid = as.numeric(strsplit(ret, ": ")[[1]][2])

#create child folder for Sub Scenarios
args = list(create=NULL, folder=NULL, lib=libpath, name="sub-scenarios", tpid=projectId(myProject))
ret = command(args, session=mySession)
sub_scn_fid = as.numeric(strsplit(ret, ": ")[[1]][2])

#create child folder for Test Scenarios
args = list(create=NULL, folder=NULL, lib=libpath, name="test-scenarios", tpid=projectId(myProject))
ret = command(args, session=mySession)
test_scn_fid = as.numeric(strsplit(ret, ": ")[[1]][2])















#move the scenario there
args = list(move=NULL, scenario=NULL, lib=libpath, sid=scenarioId(myScenario), tfid=working_scn_fid)
command(args, session=mySession)

# Definitions -------------------------------------------------------------


##### Definitions ST-Sim Terminology #####
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

##### Definitions ST-Sim Primary Strata #####
sheetName <- "stsim_Stratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir,"definitions/ecoregions.csv")) %>% filter(ID %in% c(1,4,5,6,7,8,9,13,14,78,80,81,85))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Secondary Strata ##### 
sheetName <- "stsim_SecondaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir,"definitions/counties.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Tertiary Strata ##### 
sheetName <- "stsim_TertiaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/land-managers-strata.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim State Label x #####
sheetName <- "stsim_StateLabelX"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/state-label-xid.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim State Label y #####
sheetName <- "stsim_StateLabelY"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/state-label-yid.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim State Class #####
sheetName <- "stsim_StateClass"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/state-class-types.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Transition Types #####
sheetName <- "stsim_TransitionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/transition-type.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Transition Groups #####
sheetName <- "stsim_TransitionGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/transition-group.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions ST-Sim Transition Types by Group #####
sheetName <- "stsim_TransitionTypeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/transition-types-by-group.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions ST-Sim Transition Simulation Groups #####
# Only used for projection models
sheetName <- "stsim_TransitionSimulationGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/transition-simulation-groups.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions ST-Sim Transition Multiplier Types #####
sheetName <- "stsim_TransitionMultiplierType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/transition-multiplier-types.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions ST-Sim Age Type #####
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- 300
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Age Group #####
maxAge = 300
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Attribute Groups #####
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/state-attribute-group.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim State Attribute Types #####
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/state-attribute-type.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions ST-Sim Distributions #####
sheetName <- "corestime_DistributionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/distributions.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions ST-Sim External Variables #####
sheetName <- "corestime_ExternalVariableType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/external-variables.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=F)



##### Definitions SF Stock Types #####
sheetName <- "stsimsf_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/stock-type.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions SF Stock Groups #####
sheetName <- "stsimsf_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/stock-group.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

##### Definitions SF Flow Types #####
sheetName <- "stsimsf_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/flow-type.csv"))
saveDatasheet(myProject, mySheet, name=sheetName)

##### Definitions SF Flow Groups #####
sheetName <- "stsimsf_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read.csv(paste0(dataDir, "definitions/flow-group.csv"))
saveDatasheet(myProject, mySheet, name=sheetName, append=T)


















####################################
# Sub-scenario datasheets (ST-Sim) #
####################################

##### ST-Sim Run Control #####
maxTimestep <- 2016
maxIteration <- 1
minTimestep <- 2001
minIteration <- 1
myScenario <- scenario(myProject, scenario <- paste0("Run Control Reference [Spatial; 2001-2016; 1 MC]"))
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
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-urbanization.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Change
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Change]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-agchange.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Expansion
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Expansion]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-agexpansion.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Contraction
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Ag Contraction]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-agcontraction.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Urban Intensification
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Intensification]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-intensification.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Fire
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Fire]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-fire.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Drought
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Insect]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-insect.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest
myScenario <- scenario(myProject, scenario <- "Pathway Diagram [Harvest]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsim_DeterministicTransition"
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-deterministic.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName <- "stsim_Transition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "definitions/pathway-diagram-probabilistic-harvest.csv"))
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
mySheet = read.csv(paste0(dataDir, "distributions/distribution-urbanization.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Intensification]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-intensification.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Change]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet1 = read.csv(paste0(dataDir, "distributions/distribution-ag-change-cropland-pasture.csv"))
mySheet2 = read.csv(paste0(dataDir, "distributions/distribution-ag-change-pasture-cropland.csv"))
mySheet = rbind(mySheet1, mySheet2)
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Expansion]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-ag-expansion.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Ag Contraction]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-ag-contraction.csv"))
saveDatasheet(myScenario, mySheet, sheetName)


# Fire Distributions
myScenario <- scenario(myProject, scenario = "Distirbutions [Fire Historical Mean]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-fire-ecoregion-historical-mean.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Fire Annual Variability]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-fire-historical-ecoregion-variability.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Fire Severity]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-fire-historical-severity.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Fire]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Fire Historical Mean]", 
                         "Distirbutions [Fire Annual Variability]",
                         "Distirbutions [Fire Severity]"))



# Insect Distributions
myScenario <- scenario(myProject, scenario = "Distirbutions [Insect Historical Mean]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-insects-ecoregion-historical-mean.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Insect Annual Variability]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-insects-historical-ecoregion-variability.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Insect Severity]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-insects-historical-severity.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Insect]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Insect Historical Mean]", 
                         "Distirbutions [Insect Annual Variability]",
                         "Distirbutions [Insect Severity]"))




# Harvest Distributions
myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest Historical Mean]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-harvest-county-historical-mean.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest Annual Variability]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-harvest-historical-county-variability.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest Type]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_DistributionValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "distributions/distribution-harvest-historical-type.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "Distirbutions [Harvest]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Harvest Historical Mean]", 
                         "Distirbutions [Harvest Annual Variability]",
                         "Distirbutions [Harvest Type]"))




# Merge all Distribution Datasheets into single Sub-Scenario
myScenario <- scenario(myProject, scenario = "Distributions")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Distirbutions [Urbanization]", "Distirbutions [Intensification]", "Distirbutions [Ag Change]",
                         "Distirbutions [Ag Expansion]", "Distirbutions [Ag Contraction]", "Distirbutions [Fire]", "Distirbutions [Insect]", "Distirbutions [Harvest]"))


##### External Variables #####

myScenario <- scenario(myProject, scenario = "External Variables")
sheetName <- "corestime_ExternalVariableValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv(paste0(dataDir, "external-variables/external-variables-bau.csv"))
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
# myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial]")
# sheetName <- "stsim_InitialConditionsSpatial"
# mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
# mySheet[1, "StratumFileName"] <- paste0(dataDir, "/initial-conditions/final/ic-ecoregion.tif")
# mySheet[1, "SecondaryStratumFileName"] <- paste0(dataDir, "/initial-conditions/final/ic-states.tif")
# mySheet[1, "TertiaryStratumFileName"] <- paste0(dataDir, "/initial-conditions/final/ic-land-managers.tif")
# mySheet[1, "StateClassFileName"] <- paste0(dataDir, "/initial-conditions/final/ic-state-class.tif")
# mySheet[1, "AgeFileName"] <- paste0(dataDir, "/initial-conditions/final/ic-imputed-age.tif")
# saveDatasheet(myScenario, mySheet, sheetName)

# Mapped Age
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial; Mapped Age]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- paste0(prefixDir, dataDir, "initial-conditions/ic-ecoregion.tif")
mySheet[1, "SecondaryStratumFileName"] <- paste0(prefixDir, dataDir, "initial-conditions/ic-counties.tif")
mySheet[1, "TertiaryStratumFileName"] <- paste0(prefixDir, dataDir, "initial-conditions/ic-land-managers.tif")
mySheet[1, "StateClassFileName"] <- paste0(prefixDir, dataDir, "initial-conditions/ic-state-class.tif")
mySheet[1, "AgeFileName"] <- paste0(prefixDir, dataDir, "initial-conditions/ic-age.tif")
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
mySheet[1, "RasterOutputAATPTimesteps"] <- 1
mySheet[1, "RasterOutputTransitionEvents"] <- T
mySheet[1, "RasterOutputTransitionEventTimesteps"] <- 1
mySheet[1, "RasterOutputAge"] <- T
mySheet[1, "RasterOutputAgeTimesteps"] <- 1
saveDatasheet(myScenario, mySheet, sheetName)





##### Adjacency Settings/Multipliers #####

myScenario = scenario(myProject, scenario = "Adjacency Settings")
sheetName = "stsim_TransitionAdjacencySetting"
mySheet = read.csv(paste0(dataDir, "adjacency/transition-adjacency-setting.csv"))
saveDatasheet(myScenario, mySheet, sheetName)
sheetName = "stsim_TransitionAdjacencyMultiplier"
mySheet = datasheet(myScenario, sheetName, optional = T, empty = T)
mySheet = read.csv(paste0(dataDir, "adjacency/transition-adjacency-multipliers.csv"))
saveDatasheet(myScenario, mySheet, sheetName)




##### ST-Sim Transition Targets #####

# Reference targets for historical period which reference historical distributions
myScenario <- scenario(myProject, scenario = "Transition Targets [Reference]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-reference.csv"))
saveDatasheet(myScenario, mySheet, sheetName)


# Transition Targets are defined for SSP2 and SSP5

# Ag Expansion
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [Ag Expansion SSP2]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-ag-expansion-ssp2.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [Ag Expansion SSP5]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-ag-expansion-ssp5.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Ag Contraction
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [Ag Contraction SSP2]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-ag-contraction-ssp2.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [Ag Contraction SSP5]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-ag-contraction-ssp5.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Urbanization
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [Urbanization SSP2]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-urbanization-ssp2.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [Urbanization SSP5]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-urbanization-ssp5.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Intensification
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [Intensification SSP2]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-intensification-ssp2.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [Intensification SSP5]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-intensification-ssp5.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Forest Harvest
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [Harvest SSP2]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-harvest-ssp2.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [Harvest SSP5]")
sheetName <- "stsim_TransitionTarget"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-targets/transition-targets-harvest-ssp5.csv"))
saveDatasheet(myScenario, mySheet, sheetName)


# Combine subscenarios into Merged SSP Scenarios
# SSP2
myScenario <- scenario(myProject, scenario = "Transition Targets [SSP2]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Transition Targets [Reference]", "Transition Targets [Ag Expansion SSP2]", "Transition Targets [Ag Contraction SSP2]",
                         "Transition Targets [Urbanization SSP2]", "Transition Targets [Intensification SSP2]", "Transition Targets [Harvest SSP2]"))

# SSP5
myScenario <- scenario(myProject, scenario = "Transition Targets [SSP5]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Transition Targets [Reference]", "Transition Targets [Ag Expansion SSP5]", "Transition Targets [Ag Contraction SSP5]",
                         "Transition Targets [Urbanization SSP5]", "Transition Targets [Intensification SSP5]", "Transition Targets [Harvest SSP5]"))



##### ST-Sim Transition Multipliers #####

# Urbanization multipliers (converts Urbanization targets/distributions to conversions to specific types based on NLCD proportions)
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Urbanization]")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-multipliers/transition-multipliers-urbanization-types.csv"))
saveDatasheet(myScenario, mySheet, sheetName)


# Ag Expansion and Contraction multipliers (converts Ag Change targets/distributions to conversions to specific types based on NLCD proportions)
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Ag Expansion Contraction]")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = F, optional = T)
mySheet <- read.csv(paste0(dataDir, "transition-multipliers/transition-multipliers-ag-expansion-contraction-types.csv"))
saveDatasheet(myScenario, mySheet, sheetName)

# Harvest Multipliers (Allocates total Harvest Transition Target to different types of Harvest - for projection only)
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Harvest]")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Management: Forest Clearcut [Type]", DistributionType = "Harvest: Forest Clearcut"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Management: Forest Selection [Type]", DistributionType = "Harvest: Forest Selection"))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, TransitionGroupID = "Management: Forest Selection 20% [Type]", Amount = 0))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, TransitionGroupID = "Management: Forest Selection 85% [Type]", Amount = 0))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, TransitionGroupID = "Management: Salvage Logging [Type]", Amount = 0))
saveDatasheet(myScenario, mySheet, sheetName)


# Fire Projections based on historical data
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Fire]")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", TransitionMultiplierTypeID = "Historical Mean", DistributionType = "Fire: Historical Mean"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", TransitionMultiplierTypeID = "Annual Variability", DistributionType = "Fire: Annual Variability"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire: High Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Fire: High Severity"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire: Medium Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Fire: Medium Severity"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire: Low Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Fire: Low Severity"))
saveDatasheet(myScenario, mySheet, sheetName)


# Insect Projections based on historical data
myScenario <- scenario(myProject, scenario = "Transition Multipliers [Insect]")
sheetName <- "stsim_TransitionMultiplierValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Insect", TransitionMultiplierTypeID = "Historical Mean", DistributionType = "Insect: Historical Mean"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Insect", TransitionMultiplierTypeID = "Annual Variability", DistributionType = "Insect: Annual Variability"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Insect: High Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Insect: High Severity"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Insect: Medium Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Insect: Medium Severity"))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Insect: Low Severity [Type]", TransitionMultiplierTypeID = "Severity", DistributionType = "Insect: Low Severity"))
saveDatasheet(myScenario, mySheet, sheetName)



# Merge Transition Multipliers
myScenario <- scenario(myProject, scenario = "Transition Multipliers")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Transition Multipliers [Urbanization]", 
                         "Transition Multipliers [Ag Expansion Contraction]",
                         "Transition Multipliers [Harvest]",
                         "Transition Multipliers [Fire]", 
                         "Transition Multipliers [Insect]"))








##### ST-Sim Spatial Multipliers #####

# Reference Spatial Multipliers
myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Base]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2002, TransitionGroupID = "Urbanization", MultiplierFileName = paste0(prefixDir, dataDir, "spatial-multipliers/sm-urbanization.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, TransitionGroupID = "Ag Expansion", MultiplierFileName = paste0(prefixDir, dataDir, "spatial-multipliers/sm-ag-expansion.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Management: Forest Clearcut [Type]", MultiplierFileName = paste0(prefixDir, dataDir, "spatial-multipliers/sm-harvest.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Management: Forest Selection [Type]", MultiplierFileName = paste0(prefixDir, dataDir, "spatial-multipliers/sm-harvest.tif")))
saveDatasheet(myScenario, mySheet, sheetName)





# Fire
dir1 = paste0(prefixDir, dataDir, "spatial-multipliers/fire-high-severity/")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Fire: High Severity [Type]"

dir2 = paste0(prefixDir, dataDir, "spatial-multipliers/fire-medium-severity/")
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Fire: Medium Severity [Type]"

dir3 = paste0(prefixDir, dataDir, "spatial-multipliers/fire-low-severity/")
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Fire: Low Severity [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Historical Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(1999,2016,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(1999,2016,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(1999,2016,1), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg3, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
saveDatasheet(myScenario, mySheet, sheetName)



# Insect
dir1 = paste0(prefixDir, dataDir, "spatial-multipliers/insects-high-severity/")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Insect: High Severity [Type]"

dir2 = paste0(prefixDir, dataDir, "spatial-multipliers/insects-medium-severity/")
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Insect: Medium Severity [Type]"

dir3 = paste0(prefixDir, dataDir, "spatial-multipliers/insects-low-severity/")
list3 = list.files(dir3, pattern = "*.tif$")
tg3 = "Insect: Low Severity [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Historical Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(1998,2016,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(1998,2016,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(1998,2016,1), TransitionGroupID = tg3, MultiplierFileName = paste0(dir3,list3)))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg1, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg2, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = tg3, MultiplierFileName = paste0(prefixDir, dataDir,"spatial-multipliers/sm-fire-insect-reset.tif")))
saveDatasheet(myScenario, mySheet, sheetName)



# Harvest
dir1 = paste0(prefixDir, dataDir, "spatial-multipliers/clearcut/")
list1 = list.files(dir1, pattern = "*.tif$")
tg1 = "Management: Forest Clearcut [Type]"

dir2 = paste0(prefixDir, dataDir, "spatial-multipliers/selection/")
list2 = list.files(dir2, pattern = "*.tif$")
tg2 = "Management: Forest Selection [Type]"

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Historical Forest Harvest]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = seq(1999,2016,1), TransitionGroupID = tg1, MultiplierFileName = paste0(dir1,list1)))
mySheet = addRow(mySheet, data.frame(Timestep = seq(1999,2016,1), TransitionGroupID = tg2, MultiplierFileName = paste0(dir2,list2)))
saveDatasheet(myScenario, mySheet, sheetName)

# Merge Spatial Multipliers

myScenario <- scenario(myProject, scenario = "Spatial Multipliers [Historical]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Spatial Multipliers [Base]", "Spatial Multipliers [Historical Fire]",
                         "Spatial Multipliers [Historical Insect]", "Spatial Multipliers [Historical Forest Harvest]"))


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







# Transition Size Distribution -------------------------------------------------------

# Fire
myScenario = scenario(myProject, scenario = "Transition Size Distribution [Fire]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/size-distribution/size-distribution-fire.csv")
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_TransitionSizePrioritization"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(TransitionGroupID = "Fire", Priority = "Largest", MaximizeFidelityToDistribution = T))
saveDatasheet(myScenario, mySheet, sheetName)

# Insect
myScenario = scenario(myProject, scenario = "Transition Size Distribution [Insect]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/size-distribution/size-distribution-insect.csv")
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_TransitionSizePrioritization"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(TransitionGroupID = "Insect", Priority = "Largest", MaximizeFidelityToDistribution = T))
saveDatasheet(myScenario, mySheet, sheetName)

# Clearcut
myScenario = scenario(myProject, scenario = "Transition Size Distribution [Clearcut]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/size-distribution/size-distribution-clearcut.csv")
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_TransitionSizePrioritization"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(TransitionGroupID = "Management: Forest Clearcut [Type]", Priority = "Smallest", MaximizeFidelityToTotalArea = T))
saveDatasheet(myScenario, mySheet, sheetName)

# Selection
myScenario = scenario(myProject, scenario = "Transition Size Distribution [Selection]")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSizeDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read.csv("data/size-distribution/size-distribution-selection.csv")
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_TransitionSizePrioritization"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(TransitionGroupID = "Management: Forest Selection [Type]", Priority = "Smallest", MaximizeFidelityToTotalArea = T))
saveDatasheet(myScenario, mySheet, sheetName)


# Merge Transition Size Distributions
myScenario <- scenario(myProject, scenario = "Transition Size Distribution")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Transition Size Distribution [Fire]",
                         "Transition Size Distribution [Insect]",
                         "Transition Size Distribution [Clearcut]",
                         "Transition Size Distribution [Selection]"))






# Transition Slope Multipliers --------------------------------------------

# ca_dem = raster("I:/GIS-Raster/DEM/NED/conus_ned_alb_250m.img")
# ca_dem = projectRaster(ca_dem, counties)
# ca_dem = mask(ca_dem, counties)
# plot(ca_dem)
# writeRaster(ca_dem, "data/initial-conditions/ic-dem.tif", overwrite=T, format="GTiff", datatype="INT2S")
# ca_dem = raster("data/initial-conditions/ic-dem.tif")

myScenario = scenario(myProject, scenario = "Transition Slope Multipliers")
mergeDependencies(myScenario) = T
sheetName <- "stsim_TransitionSlopeMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = -90, Amount = 5))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = -16, Amount = 1))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = -0, Amount = 17))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 16, Amount = 34))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 25, Amount = 100))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 35, Amount = 200))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 45, Amount = 300))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 55, Amount = 400))
mySheet = addRow(mySheet, data.frame(Timestep = 2017, TransitionGroupID = "Fire", Slope = 90, Amount = 500))
saveDatasheet(myScenario, mySheet, sheetName)

sheetName = "stsim_DigitalElevationModel"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(DigitalElevationModelFileName = "D:/california-sig/data/initial-conditions/ic-dem.tif"))
saveDatasheet(myScenario, mySheet, sheetName)


datasheet(myScenario)





# STSIM Constants ---------------------------------------------------------


myScenario <- scenario(myProject, scenario = "STSM Constants")
mergeDependencies(myScenario) = T
dependency(myScenario, c("Run Control Reference [Spatial; 2001-2016; 1 MC]",
                         "Output Options [Spatial]",
                         "Initial Conditions [Spatial; Mapped Age]",
                         "Adjacency Settings",
                         "External Variables",
                         "Pathway Diagram",
                         "Distributions",
                         "Transition Multipliers",
                         "Transition Size Distribution",
                         "Transition Slope Multipliers",
                         "State Attributes with Harvest"))






# Test Scenario -----------------------------------------------------------

myScenario <- scenario(myProject, scenario = "Historical Scenario [2001-2020; 1 MC]")
mergeDependencies(myScenario) = T
dependency(myScenario, c("STSM Constants",
                         "Spatial Multipliers [Historical]",
                         "Transition Targets [Reference]"))





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

