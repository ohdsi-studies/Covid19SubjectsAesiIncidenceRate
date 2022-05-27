# ##############################################################################
# Adverse Events of Special Interest within COVID-19 Subjects Reporting
# ##############################################################################

# PREP & VARIABLES #############################################################
setwd("D:\\Git\\BitBucket\\epi_974\\Covid19SubjectsAesiIncidenceRate\\extras\\CovidAesiReporting")

library(dplyr)
library(tidyr)
library(CohortDiagnostics)

options(scipen=999)

cohortDiagnosticsFolder <- "data/cohortDiagnostics" #where you store the CD results
irFolder <- "data/ir" #where you store the IR results
incidenceAnalysisFile <- paste0(irFolder,"/incidenceAnalysisCensored.csv")
censorshipFile <- "extras/Censorship Worksheet.xlsx"
resultsFolder <- "results" #where you want to write to

resultsSchema <- 'covid_aesi' #schema to load CohortDiagnostic results in OHDSI

outcomeNameSortOrder <- seq(1,16)
outcomeName <- c('Acute Myocardial Infarction','Non-hemorrhagic Stroke',
                 'Deep Vein Thrombosis (DVT)','Pulmonary Embolism',
                 'Hemorrhagic Stroke','Bells Palsy','Appendicitis',
                 'Myocarditis Pericarditis','Thrombosis with Thrombocytopenia (TWT)',
                 'Immune Thrombocytopenia (ITP)','Anaphylaxis','Narcolepsy',
                 'Disseminated Intravascular Coagulation','Encephalomyelitis',
                 'Guillain Barre Syndrome','Transverse Myelitis')
outcomeSortOrder <- data.frame(outcomeNameSortOrder, outcomeName)

# GET DATA #####################################################################
## Cohort Diagnostics-----------------------------------------------------------
#takes data from CD zips
getCohortDiagnostics(cohortDiagnosticsFolder)

#Launch locally
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = "D:\\Git\\BitBucket\\epi_974\\Covid19SubjectsAesiIncidenceRate\\extras\\CovidAesiReporting\\data\\cohortDiagnostics")

## IR ---------------------------------------------------------------------------
#pulls together IR results into one file, forces min cell count,
#DP name clean up, output to CSV, returns incidenceAnalysis dataframe
incidenceAnalysis <- getDataIR(irFolder)

## CLEAN & CENSOR IR DATA #######################################################

#incidenceAnalysis <- read.csv(paste0(irFolder,"/incidenceAnalysis.csv"))
load(paste0(irFolder,"/PreMerged.RData"))

# CENSORSHIP PREP
notCensoredOutomesDF <- whatOutcomesToCensor(dataFolder = irFolder, censorshipFile)
censorSubgroupCohortDefinitionId <- whatsubgroupsToCensor()

# IR CLEANUP
incidenceAnalysisCensored <- cleanAndApplyCensor(irDf = incidenceAnalysis,
                                                 notCensoredOutomesDF,
                                                 censorSubgroupCohortDefinitionId,
                                                 dataFolder= irFolder,
                                                 outcomeSortOrder)
write.csv(incidenceAnalysisCensored,paste0(irFolder,"/incidenceAnalysisCensored.csv"), row.names = FALSE)

#trying to find where people deleted rows, not successful yet.
# tarCount <- length(unique(incidenceAnalysisCensored$timeAtRiskId))
# subgroupCount <- length(unique(incidenceAnalysisCensored$subgroupCohortDefinitionId))
# aesiPerDb <- unique(incidenceAnalysisCensored[,c('databaseName', "outcomeName","targetName")])
# aesiTargetCountPerDb <- aesiPerDb %>% count(databaseName)
# aesiTargetCountPerDb$rowCount <- aesiCountPerDb$n * tarCount * subgroupCount
# irCounts <- incidenceAnalysisCensored %>% count(databaseName, sort = TRUE)
# df <- merge(aesiTargetCountPerDb, irCounts, by = c('databaseName'))


# META ANALYSIS & PRETTY TABLE #################################################
#runs the meta analysis and makes the pretty table for the paper
metaAnalysis(resultsFolder,incidenceAnalysisFile,outcomeSortOrder)

# STANDARDIZED INCIDENCE RATE RATIOS ###########################################

covidPop <- c(563)
generalPop <- c(566)
tar <- c(6)
subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92) #all groups except All and pediatrics

sirr(covidPop,generalPop,tar,subgroup,dataFolder = irFolder,resultsFolder)


# SIRR META-ANALYSIS FOREST PLOT WITH DB RATES #################################

#figure out what was censored
dataFolder <- irFolder
load(paste0(irFolder,"/PreMerged.RData"))
cohorts <- distinct(incidenceAnalysis[,c("outcomeCohortDefinitionId","outcomeName")])
cohorts <- cohorts[cohorts$outcomeCohortDefinitionId != 568,] #remove second anaphalaxis
censoredOutcomes <- whatOutcomesToCensor(dataFolder, censorshipFile,censoredResults=1)
censoredOutcomes <- merge(x = censoredOutcomes, y = cohorts, by.x = "PHENOTYPE", by.y = "outcomeCohortDefinitionId", all.x = TRUE)
colnames(censoredOutcomes) <- c("PHENOTYPE","databaseName","CENSORED","outcomeName")
censoredOutcomesToAppend <- as.data.frame(cbind(censoredOutcomes$databaseName,rep(6,nrow(censoredOutcomes)),
                                                censoredOutcomes$outcomeName,rep(9999,nrow(censoredOutcomes)),
                                                rep(9999,nrow(censoredOutcomes)),rep(9999,nrow(censoredOutcomes)),
                                                rep(9999,nrow(censoredOutcomes)),rep("Censored",nrow(censoredOutcomes)),
                                                rep(9999,nrow(censoredOutcomes)),rep(9999,nrow(censoredOutcomes)),
                                                rep(9999,nrow(censoredOutcomes)),rep(9999,nrow(censoredOutcomes))))
colnames(censoredOutcomesToAppend) <- c("databaseName","timeAtRiskId","outcomeName","generalWeightedIRTotal",
                                        "covidWeightedIRTotal","covidE","covidD","SIR","SIRLB","SIRUB",
                                        "logSIR","logSIRSE" )
censoredOutcomesToAppend <- transform(censoredOutcomesToAppend,timeAtRiskId = as.integer(timeAtRiskId),
                                      generalWeightedIRTotal = as.numeric(generalWeightedIRTotal),
                                      covidWeightedIRTotal = as.numeric(covidWeightedIRTotal),
                                      covidE = as.numeric(covidE),
                                      covidD = as.integer(covidD),
                                      SIR = as.numeric(SIR),
                                      SIRLB = as.numeric(SIRLB),
                                      SIRUB = as.numeric(SIRUB),
                                      logSIR  = as.numeric(logSIR),
                                      logSIRSE = as.numeric(logSIRSE))

#add censored records to records with values
summaryIR <- read.csv(paste0(resultsFolder,"/summaryIR.csv"))
aesis <- unique(summaryIR$out)
summaryIR <- rbind(summaryIR,censoredOutcomesToAppend)
summaryIR <- summaryIR %>% drop_na(outcomeName)
#clean up issue with Anaphylaxis twice
summaryIRAnaphylaxis <- summaryIR[summaryIR$outcomeName == "Anaphylaxis",]
summaryIRAnaphylaxisCounts <- summaryIRAnaphylaxis %>% group_by(databaseName) %>% tally()
summaryIRAnaphylaxisDuplicated <- summaryIRAnaphylaxisCounts[summaryIRAnaphylaxisCounts$n == 2,]
summaryIRFilter <- summaryIR %>% filter(summaryIR$outcomeName == "Anaphylaxis" & summaryIR$databaseName %in% summaryIRAnaphylaxisDuplicated$databaseName & summaryIR$generalWeightedIRTotal == 9999)
summaryIR <- sqldf::sqldf('SELECT * FROM summaryIR EXCEPT SELECT * FROM summaryIRFilter')

metaAnalysisIR <- read.csv(paste0(resultsFolder,"/metaResults.csv"))

for(i in 1:length(aesis)){
  sirrForestPlots(summaryIR, metaAnalysisIR, aesi = aesis[i])
}

# AGE & SEX STRATIFIED IR PLOT #################################################

ageSexStratifiedPlot(resultsFolder, irFolder)

# COVID IR vs GENDERAL IRSTRATIFIED IR PLOT ####################################

incidenceAnalysisCensoredCovidVsGeneral <- read.csv(paste0(irFolder,"/incidenceAnalysisCensoredCovidVsGeneral.csv"))

subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92)

incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneral[incidenceAnalysisCensoredCovidVsGeneral$timeAtRiskId == 6,]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[incidenceAnalysisCensoredCovidVsGeneralForPlot$subgroupName != 'All',]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[incidenceAnalysisCensoredCovidVsGeneralForPlot$subgroupCohortDefinitionId %in% subgroup,]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[,c('databaseName','targetName','timeAtRiskId','subgroupName',"gender","ageGroup",'outcomeName',"incidenceRateP100py","incidenceRateP100pyGeneral")]

write.csv(incidenceAnalysisCensoredCovidVsGeneralForPlot,paste0(irFolder,"/incidenceAnalysisCensoredCovidVsGeneralForPlot.csv"), row.names = FALSE)

# SIRR META-ANALYSIS FOREST PLOT SUMMARY #######################################

metaAnalysisIR <- read.csv(paste0(resultsFolder,"/metaResults.csv"))
metaAnalysisIR <- metaAnalysisIR[order(-metaAnalysisIR$SIR),]
metaAnalysisForestPlots(metaAnalysisIR,resultsFolder)

