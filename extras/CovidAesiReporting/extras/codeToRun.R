# ##############################################################################
# Adverse Events of Special Interest within COVID-19 Subjects Reporting
# ##############################################################################

# PREP & VARIABLES #############################################################
library(dplyr)
library(tidyr)
library(CohortDiagnostics)

options(scipen=999)

cohortDiagnosticsFolder <- "data/cohortDiagnostics" #where you store the CD results
irFolder <- "data/ir" #where you store the IR results
incidenceAnalysisFile <- paste0(ir,"/incidenceAnalysisCensored.csv")
censorshipFile <- "extras/Censorship Worksheet.xlsx"
resultsFolder <- "results" #where you want to write to

resultsSchema <- 'covid_aesi' #schema to load CohortDiagnostic results in OHDSI

# GET DATA #####################################################################
## Cohort Diagnostics-----------------------------------------------------------
#takes data from CD zips and loads them to OHDSI server
getCohortDiagnostics(cohortDiagnosticsFolder)

#Launch locally using OHDSI Server
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = paste(Sys.getenv("shinydbServer"),Sys.getenv("shinydbDatabase"),sep = "/"),
  port = Sys.getenv("shinydbPort"),
  user = Sys.getenv("shinydbUser"),
  password = Sys.getenv("shinydbPW")
)

CohortDiagnostics::launchDiagnosticsExplorer(connectionDetails = connectionDetails,
                                             resultsDatabaseSchema = resultsSchema)
## IR ---------------------------------------------------------------------------
#pulls together IR results into one file, forces min cell count,
#DP name clean up, output to CSV, returns incidenceAnalysis dataframe
incidenceAnalysis <- getDataIR(irFolder)

# CLEAN & CENSOR IR DATA #######################################################

#incidenceAnalysis <- read.csv(paste0(irFolder,"/incidenceAnalysis.csv"))
load(paste0(irFolder,"/PreMerged.RData"))

# CENSORSHIP PREP
notCensoredOutomesDF <- whatOutcomesToCensor(dataFolder = irFolder, censorshipFile)
censorSubgroupCohortDefinitionId <- whatsubgroupsToCensor()

# IR CLEANUP
incidenceAnalysisCensored <- cleanAndApplyCensor(incidenceAnalysis, notCensoredOutomesDF, censorSubgroupCohortDefinitionId)
write.csv(incidenceAnalysisCensored,paste0(irFolder,"/incidenceAnalysisCensored.csv"), row.names = FALSE)

# META ANALYSIS & PRETTY TABLE #################################################
#runs the meta analysis and makes the pretty table for the paper
metaAnalysis(resultsFolder,incidenceAnalysisFile)

# STANDARDIZED INCIDENCE RATE RATIOS ###########################################

covidPop <- c(563)
generalPop <- c(566)
tar <- c(6)
subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92) #all groups except All and pediatrics

sirr(covidPop,generalPop,tar,subgroup,dataFolder = irFolder,resultsFolder)

# SIRR META-ANALYSIS FOREST PLOT ###############################################

summaryIR <- read.csv(paste0(resultsFolder,"/summaryIR.csv"))
metaAnalysisIR <- read.csv(paste0(resultsFolder,"/metaResults.csv"))
aesis <- unique(summaryIR$outcomeName)

for(i in 1:length(aesis)){
  sirrForestPlots(summaryIR, metaAnalysisIR, aesi = aesis[i])
}

# AGE & SEX STRATIFIED IR PLOT #################################################

ageSexStratifiedPlot(resultsFolder, irFolder)


# SIRR META-ANALYSIS FOREST PLOT ###############################################

metaAnalysisIR <- read.csv(paste0(resultsFolder,"/metaResults.csv"))
metaAnalysisForestPlots(metaAnalysisIR)


