getDataIR <- function(irFolder){
  dataFolder <- irFolder
  specificationFileName <- "resultsDataModelSpecificationForIR.csv"
  preMergeIRFiles(dataFolder, specificationFileName, tempFolder = tempdir())
  load(paste0(dataFolder,"/PreMerged.RData"))

  # CLEAN
  minCellCount = 5
  fieldsToCensor <- c("numPersonsWOutcomePreExclude",
                      "numPersonsWOutcome",
                      "numOutcomesPreExclude",
                      "numOutcomes",
                      "numPersonsPreExclude",
                      "numPersonsAtRisk")

  for (i in 1:length(fieldsToCensor)) {
    incidenceAnalysis <- enforceMinCellValue(incidenceAnalysis, fieldsToCensor[i], minValues = minCellCount)
  }

  incidenceAnalysis[incidenceAnalysis == "Institut Municipal Assistència Sanitària Information System
Parc Salut Mar Barcelona, Hospital del Mar (IMIM)"] <- "Parc de Salut Mar Barcelona Information System"
  incidenceAnalysis[incidenceAnalysis == "he IQVIA¨ Adjudicated Health Plan Claims Data"] <- "IQVIA Adjudicated Health Plan Claims Data"
  incidenceAnalysis[incidenceAnalysis == "IBM MarketScan Medicare Supplemental and Coordination of Benefits Database"] <- "IBM(R) MarketScan Medicare Supplemental and Coordination of Benefits Database"
  incidenceAnalysis <- distinct(incidenceAnalysis)

  # WRITE
  write.csv(unique(incidenceAnalysis),paste0(dataFolder,"/incidenceAnalysis.csv"), row.names = FALSE)
  write.csv(database,paste0(dataFolder,"/database.csv"), row.names = FALSE)
  write.csv(package,paste0(dataFolder,"/package.csv"), row.names = FALSE)

  return(incidenceAnalysis )
}
