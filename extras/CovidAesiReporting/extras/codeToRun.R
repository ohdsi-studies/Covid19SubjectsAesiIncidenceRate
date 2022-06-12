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

#things to run negative controls
irFolderNegativeControls <- "data/ir_negativeControls" #where you store the IR results
censorshipFileNegativeControls <- "extras/Censorship Worksheet Negative Controls.xlsx"
resultsFolderNegativeControls <- "resultsNegativeControls" #where you want to write to
incidenceAnalysisFileNegativeControls <- paste0(irFolderNegativeControls,"/incidenceAnalysisCensored.csv")

outcomeNameSortOrder <- seq(1,20)
outcomeName <- c('[NC] Alcoholic liver damage','[NC] Animal bite wound','[NC] Ankle ulcer','[NC] Benign neoplasm of ovary','[NC] Biliary calculus','[NC] Burn of digit of hand','[NC] Burn of lower leg','[NC] Cannabis abuse','[NC] Cervical spine ankylosis','[NC] Contusion of toe','[NC] Endometriosis of uterus','[NC] Hyperplasia of prostate','[NC] Intestinal parasitism','[NC] Leukemia','[NC] Open wound of buttock','[NC] Poisoning by bee sting','[NC] Primary malignant neoplasm of pancreas','[NC] Prosthetic joint loosening','[NC] Sprain of spinal ligament','[NC] Tailor?s bunion')
outcomeSortOrderNegativeControls <- data.frame(outcomeNameSortOrder, outcomeName)

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

## CLEAN & CENSOR IR DATA ######################################################

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

# META ANALYSIS & PRETTY TABLE #################################################
#runs the meta analysis and makes the pretty table for the paper
metaAnalysis(resultsFolder,incidenceAnalysisFile,outcomeSortOrder)

# STANDARDIZED INCIDENCE RATE RATIOS ###########################################

covidPop <- c(563)
generalPop <- c(566)
tar <- c(6)
subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92) #all groups except All and pediatrics

sirr(covidPop,generalPop,tar,subgroup,dataFolder = irFolder,resultsFolder,irFile = "incidenceAnalysisCensored.csv")


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
  sirrForestPlots(resultsFolder, summaryIR, metaAnalysisIR, aesi = aesis[i])
}

# COVID IR vs GENDERAL IRSTRATIFIED IR PLOT ####################################

incidenceAnalysisCensoredCovidVsGeneral <- read.csv(paste0(irFolder,"/incidenceAnalysisCensoredCovidVsGeneral.csv"))

subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92)

incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneral[incidenceAnalysisCensoredCovidVsGeneral$timeAtRiskId == 6,]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[incidenceAnalysisCensoredCovidVsGeneralForPlot$subgroupName != 'All',]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[incidenceAnalysisCensoredCovidVsGeneralForPlot$subgroupCohortDefinitionId %in% subgroup,]
incidenceAnalysisCensoredCovidVsGeneralForPlot <- incidenceAnalysisCensoredCovidVsGeneralForPlot[,c('databaseName','targetName','timeAtRiskId','subgroupName',"gender","ageGroup",'outcomeName',"incidenceRateP100py","incidenceRateP100pyGeneral")]

write.csv(incidenceAnalysisCensoredCovidVsGeneralForPlot,paste0(irFolder,"/incidenceAnalysisCensoredCovidVsGeneralForPlot.csv"), row.names = FALSE)

# AGE & SEX STRATIFIED IR PLOT & COVID IR vs General (FIGURE 1 & 3) ############

ageSexStratifiedPlot(resultsFolder, irFolder)

# SIRR META-ANALYSIS FOREST PLOT SUMMARY #######################################

metaAnalysisIR <- read.csv(paste0(resultsFolder,"/metaResults.csv"))
metaAnalysisIR <- metaAnalysisIR[order(-metaAnalysisIR$SIR),]
metaAnalysisForestPlots(metaAnalysisIR,resultsFolder)

# SIRR META-ANALYSIS FOREST PLOT SUMMARY #######################################

# NEGATIVE CONTROLS ############################################################
## GET DATA ####################################################################
### IR -------------------------------------------------------------------------
incidenceAnalysis <- getDataIR(irFolderNegativeControls)

## CLEAN & CENSOR IR DATA ######################################################
#incidenceAnalysis <- read.csv(paste0(irFolder,"/incidenceAnalysis.csv"))
load(paste0(irFolderNegativeControls,"/PreMerged.RData"))

# CENSORSHIP PREP
notCensoredOutomesDF <- whatOutcomesToCensor(dataFolder = irFolderNegativeControls, censorshipFileNegativeControls,negativeControls=1)
censorSubgroupCohortDefinitionId <- whatsubgroupsToCensor()

# IR CLEANUP
incidenceAnalysisCensored <- cleanAndApplyCensor(irDf = incidenceAnalysis,
                                                 notCensoredOutomesDF,
                                                 censorSubgroupCohortDefinitionId,
                                                 dataFolder= irFolderNegativeControls,
                                                 outcomeSortOrderNegativeControls)
write.csv(incidenceAnalysisCensored,paste0(irFolderNegativeControls,"/incidenceAnalysisCensored.csv"), row.names = FALSE)

## STANDARDIZED INCIDENCE RATE RATIOS ##########################################
covidPop <- c(563)
generalPop <- c(566)
tar <- c(6)
subgroup <- c(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92) #all groups except All and pediatrics

sirr(covidPop,generalPop,tar,subgroup,dataFolder = irFolderNegativeControls,resultsFolderNegativeControls,irFile = "incidenceAnalysisCensored.csv")


## SIRR META-ANALYSIS FOREST PLOT WITH DB RATES ################################

#figure out what was censored
dataFolder <- irFolderNegativeControls
load(paste0(irFolderNegativeControls,"/PreMerged.RData"))
cohorts <- distinct(incidenceAnalysis[,c("outcomeCohortDefinitionId","outcomeName")])
cohorts <- cohorts[cohorts$outcomeCohortDefinitionId != 568,] #remove second anaphalaxis
censoredOutcomes <- whatOutcomesToCensor(dataFolder, censorshipFileNegativeControls,censoredResults=1,negativeControls = 1)
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
summaryIR <- read.csv(paste0(resultsFolderNegativeControls,"/summaryIR.csv"))
aesis <- unique(summaryIR$out)
summaryIR <- rbind(summaryIR,censoredOutcomesToAppend)
summaryIR <- summaryIR %>% drop_na(outcomeName)
#clean up issue with Anaphylaxis twice
summaryIRAnaphylaxis <- summaryIR[summaryIR$outcomeName == "Anaphylaxis",]
summaryIRAnaphylaxisCounts <- summaryIRAnaphylaxis %>% group_by(databaseName) %>% tally()
summaryIRAnaphylaxisDuplicated <- summaryIRAnaphylaxisCounts[summaryIRAnaphylaxisCounts$n == 2,]
summaryIRFilter <- summaryIR %>% filter(summaryIR$outcomeName == "Anaphylaxis" & summaryIR$databaseName %in% summaryIRAnaphylaxisDuplicated$databaseName & summaryIR$generalWeightedIRTotal == 9999)
summaryIR <- sqldf::sqldf('SELECT * FROM summaryIR EXCEPT SELECT * FROM summaryIRFilter')

metaAnalysisIR <- read.csv(paste0(resultsFolderNegativeControls,"/metaResults.csv"))

for(i in 1:length(aesis)){
  sirrForestPlots(resultsFolder = resultsFolderNegativeControls,summaryIR, metaAnalysisIR, aesi = aesis[i])
}

# SIRR META-ANALYSIS FOREST PLOT SUMMARY #######################################

metaAnalysisIR <- read.csv(paste0(resultsFolderNegativeControls,"/metaResults.csv"))
metaAnalysisIR$outcomeName <- stringr::str_replace_all(string = metaAnalysisIR$outcomeName,'[?]',"'")
metaAnalysisIR$outcomeName <- stringr::str_replace_all(string = metaAnalysisIR$outcomeName,'\\[NC\\] ',"")
metaAnalysisIR <- metaAnalysisIR[order(-metaAnalysisIR$SIR),]
metaAnalysisForestPlots(metaAnalysisIR,resultsFolderNegativeControls)
