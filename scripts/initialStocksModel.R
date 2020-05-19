
library(raster)
library(tidyverse)
library(rsyncrosim)

# Create a simple ST-Sim library which will calculate spatial initial conditions maps from non-spatial input
SyncroSimDir <- "C:/Program Files/SyncroSim/"
mySession <- session(SyncroSimDir)

ssimDir = "F:/national-assessment/data/ssim-libs/"

myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model Conus.ssim"), session = mySession)
#myLibrary = ssimLibrary(name = paste0(ssimDir,"Initial Stocks Model.ssim"), session = mySession)
myProject = project(myLibrary, project="Initial Stocks")



########################
# Definitions (ST-Sim) #
########################
# ST-Sim Terminology
sheetName <- "stsim_Terminology"
mySheet <- datasheet(myProject, name=sheetName)
mySheet$AmountLabel[1] <- "Area"
mySheet$AmountUnits[1] <- "Hectares"
mySheet$StateLabelX[1] <- "LULC"
mySheet$StateLabelY[1] <- "Subclass"
mySheet$PrimaryStratumLabel[1] <- "Ecological Boundary"
mySheet$SecondaryStratumLabel[1] <- "Administrative Boundary"
mySheet$TimestepUnits[1] <- "Year"
saveDatasheet(myProject, mySheet, sheetName)

# ST-Sim Primary Strata 
sheetName <- "stsim_Stratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/ecoregions.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim Secondary Strata 
sheetName <- "stsim_SecondaryStratum"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/states.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Label x
sheetName <- "stsim_StateLabelX"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/state-label-xid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Label y
sheetName <- "stsim_StateLabelY"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/state-label-yid.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

# ST-Sim State Class
sheetName <- "stsim_StateClass"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/state-class-types.csv", col_types = cols(.default = "c"))
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Age Type
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- 300
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Age Group
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Attribute Groups
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/state-attribute-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#ST-Sim Attribute Types
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/state-attribute-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Stock Types
sheetName <- "stsimsf_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/stock-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Stock Groups
sheetName <- "stsimsf_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/stock-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)

#SF Flow Types
sheetName <- "stsimsf_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/flow-type.csv")
saveDatasheet(myProject, mySheet, name=sheetName)

#SF Flow Groups
sheetName <- "stsimsf_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet = read_csv("F:/national-assessment/data/definitions/flow-group.csv")
saveDatasheet(myProject, mySheet, name=sheetName, append=T)


datasheet(myProject)

# NB: No transitions for now

####################################
# Sub-scenario datasheets (ST-Sim) #
####################################

# ST-Sim Run Control
maxTimestep <- 1
maxIteration <- 1
minTimestep <- 0
minIteration <- 1
myScenario <- scenario(myProject, scenario <- paste0("Run Control [Spatial; ", maxTimestep, " years; ", maxIteration, " MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

# ST-Sim Transition Pathways
myScenario <- scenario(myProject, scenario <- "Pathway Diagram")
sheetName <- "stsim_DeterministicTransition"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/definitions/pathway-diagram-deterministic.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# NB: No probabilistic transitions for now


# ST-Sim Initial conditions
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

# CONUS Model
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial; Imputed Age]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-ecoregion.tif"
mySheet[1, "SecondaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-states.tif"
mySheet[1, "TertiaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-land-managers.tif"
mySheet[1, "StateClassFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-state-class.tif"
mySheet[1, "AgeFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-imputed-age.tif"
saveDatasheet(myScenario, mySheet, sheetName)

# CONUS Model
myScenario <- scenario(myProject, scenario = "Initial Conditions [Spatial; Mapped Age]")
sheetName <- "stsim_InitialConditionsSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "StratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-ecoregion.tif"
mySheet[1, "SecondaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-states.tif"
mySheet[1, "TertiaryStratumFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-land-managers.tif"
mySheet[1, "StateClassFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-state-class.tif"
mySheet[1, "AgeFileName"] <- "F:/national-assessment/data/initial-conditions/final/ic-age.tif"
saveDatasheet(myScenario, mySheet, sheetName)

# ST-Sim Output options
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
saveDatasheet(myScenario, mySheet, sheetName)


# ST-Sim State Attribute Values
myScenario <- scenario(myProject, scenario = "State Attributes Fire")
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/state-attributes/state-attribute-values-fire.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# ST-Sim State Attribute Values
myScenario <- scenario(myProject, scenario = "State Attributes Harvest")
sheetName <- "stsim_StateAttributeValue"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/state-attributes/state-attribute-values-harvest.csv") 
saveDatasheet(myScenario, mySheet, sheetName)

# SF Initial Stocks
myScenario <- scenario(myProject, scenario = "SF Initial Stocks [Non Spatial]")
sheetName <- "stsimsf_InitialStockNonSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/initial-stocks-non-spatial.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Pathways Diagram
myScenario <- scenario(myProject, scenario = "Flow Pathways")
sheetName <- "stsimsf_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/flow-pathway-diagram.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Pathways Diagram
myScenario <- scenario(myProject, scenario = "Flow Pathways")
sheetName <- "stsimsf_FlowPathway"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/flow-pathways.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Stock Group Membership
myScenario <- scenario(myProject, scenario = "SF Stock Group Membership")
sheetName <- "stsimsf_StockTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/stock-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Group Membership
myScenario <- scenario(myProject, scenario = "SF Flow Group Membership")
sheetName <- "stsimsf_FlowTypeGroupMembership"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/flow-type-group-membership.csv")
saveDatasheet(myScenario, mySheet, sheetName)

# SF Flow Flow Order
myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrder"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = read_csv("F:/national-assessment/data/stock-flow-model/flow-order.csv")
saveDatasheet(myScenario, mySheet, sheetName)

myScenario <- scenario(myProject, scenario = "SF Flow Order")
sheetName <- "stsimsf_FlowOrderOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"ApplyEquallyRankedSimultaneously"] = T
saveDatasheet(myScenario, mySheet, sheetName)

# SF Output Options
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




datasheet(myScenario)

#############################
# Full scenarios (STSim SF) #
#############################
myScenarioName <- "Initial Spatial Stock Simulation Fire [Imputed Age]"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control [Spatial; 1 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes Fire",
             "Flow Pathways",
             "SF Initial Stocks [Non Spatial]",
             "SF Output Options",
             "SF Flow Order",
             "SF Stock Group Membership",
             "SF Flow Group Membership"))

myScenarioName <- "Initial Spatial Stock Simulation Harvest [Imputed Age]"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control [Spatial; 1 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Imputed Age]",
             "Output Options [Spatial]",
             "State Attributes Harvest",
             "Flow Pathways",
             "SF Initial Stocks [Non Spatial]",
             "SF Output Options",
             "SF Flow Order",
             "SF Stock Group Membership",
             "SF Flow Group Membership"))


myScenarioName <- "Initial Spatial Stock Simulation Fire [Mapped Age]"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control [Spatial; 1 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Mapped Age]",
             "Output Options [Spatial]",
             "State Attributes Fire",
             "Flow Pathways",
             "SF Initial Stocks [Non Spatial]",
             "SF Output Options",
             "SF Flow Order",
             "SF Stock Group Membership",
             "SF Flow Group Membership"))

myScenarioName <- "Initial Spatial Stock Simulation Harvest [Mapped Age]"
myScenario = scenario(myProject, scenario = myScenarioName)
dependency(myScenario, 
           c("Run Control [Spatial; 1 years; 1 MC]",
             "Pathway Diagram",
             "Initial Conditions [Spatial; Mapped Age]",
             "Output Options [Spatial]",
             "State Attributes Harvest",
             "Flow Pathways",
             "SF Initial Stocks [Non Spatial]",
             "SF Output Options",
             "SF Flow Order",
             "SF Stock Group Membership",
             "SF Flow Group Membership"))














# SF Flow Spatial Multipliers
indir = "F:/national-assessment/data/flow-spatial-multipliers/growth/"
gcm = "bcc-csm1-1"
rcp = "rcp45"

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Growth]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/growth/bcc-csm1-1/rcp45/california", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2099), FlowGroupID = "Net Growth: Total", MultiplierFileName = paste(indir,gcm,"/",rcp,"/california/",list, sep=""))
saveDatasheet(myScenario, mySheet, sheetName)

indir = "F:/national-assessment/data/flow-spatial-multipliers/q10Fast/"
gcm = "bcc-csm1-1"
rcp = "rcp45"

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Fast]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/q10Fast/bcc-csm1-1/rcp45/california", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2099), FlowGroupID = "Q10 Fast Flows", MultiplierFileName = paste(indir,gcm,"/",rcp,"/california/",list, sep=""))
saveDatasheet(myScenario, mySheet, sheetName)

indir = "F:/national-assessment/data/flow-spatial-multipliers/q10Slow/"
gcm = "bcc-csm1-1"
rcp = "rcp45"

myScenario <- scenario(myProject, scenario = "SF Flow Spatial Multipliers [Q10 Slow]")
mergeDependencies(myScenario) = TRUE
sheetName <- "stsimsf_FlowSpatialMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
list = list.files(path = "F:/national-assessment/data/flow-spatial-multipliers/q10Slow/bcc-csm1-1/rcp45/california", pattern = "*.tif")
mySheet = data.frame(Timestep = seq(2002,2099), FlowGroupID = "Q10 Slow Flows", MultiplierFileName = paste(indir,gcm,"/",rcp,"/california/",list, sep=""))
saveDatasheet(myScenario, mySheet, sheetName)

myScenario = scenario(myProject, scenario = "SF Flow Spatial Multipliers")
mergeDependencies(myScenario) = TRUE
dependency(myScenario, c("SF Flow Spatial Multipliers [Growth]", "SF Flow Spatial Multipliers [Q10 Fast]", "SF Flow Spatial Multipliers [Q10 Slow]"))



# SF Flow Multipliers
myScenario <- scenario(myProject, scenario = "SF Flow Multipliers")
sheetName <- "stsimsf_FlowMultiplier"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Net Growth: Total", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Fast Flows", Value = 0.01))
mySheet = addRow(mySheet, data.frame(Timestep = 2002, FlowGroupID = "Q10 Slow Flows", Value = 0.01))
saveDatasheet(myScenario, mySheet, sheetName)
