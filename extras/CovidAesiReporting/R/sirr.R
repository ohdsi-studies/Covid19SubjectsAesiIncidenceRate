sirr <- function(covidPop,generalPop,tar,subgroup,dataFolder,resultsFolder,irFile){
  #takes as inputs (1) what target cohort you want, (2) the comparison cohort, (3) the time at risk, (4) and the subgroups you want to include
  #then standardizes the IR and creates IR ratios per database per AESI
  #it also creates a summary rate for all databases
  #outputs two files of IRR

  df <- read.csv(paste0(dataFolder,"/",irFile))

  df$numOutcomesUnblinded <- case_when(df$numPersonsWOutcome > 0 ~ round(df$numOutcomes,0),
                                       !is.na(df$incidenceProportionP100p) ~ round(df$numPersonsAtRisk * (df$incidenceProportionP100p/100),0),
                                       TRUE ~ 0)

  #get general population
  generalDF <- filter(df, targetCohortDefinitionId %in% generalPop & timeAtRiskId %in% tar & subgroupCohortDefinitionId %in% subgroup & numPersonsAtRisk > 0)

  generalGroupSummary <- setNames(aggregate(generalDF$numPersonsAtRisk ,
                                            list(generalDF$databaseName, generalDF$timeAtRiskId, generalDF$outcomeCohortDefinitionId),
                                            FUN=sum, na.rm=TRUE),
                                  c("databaseName", "timeAtRiskId", "outcomeCohortDefinitionId", "numPersonsAtRiskTotal"))

  #get AESI
  covidDF <- filter(df, targetCohortDefinitionId %in% covidPop  & timeAtRiskId %in% tar & subgroupCohortDefinitionId %in% subgroup)


  #pull together
  generalCovidDF <- sqldf::sqldf("SELECT DISTINCT
                                    o.databaseName,
                                    o.timeAtRiskId,
                                    o.outcomeCohortDefinitionId,
                                    o.outcomeName,
                                    o.subgroupCohortDefinitionId,
                                    o.subgroupName,
                                    o.gender,
                                    o.ageGroup,
                                    o.numOutcomesUnblinded AS generalNumOutcomesUnblinded,
                                    o.personYears AS generalPersonYears,
                                    o.incidenceRateP100py AS generalIncidenceRateP100py,
                                    o.numPersonsAtRisk*1.0/gs.numPersonsAtRiskTotal AS genderalWeight,
                                    o.numPersonsAtRisk*1.0/gs.numPersonsAtRiskTotal * o.incidenceRateP100PY AS generalWeightedIR,
                                    t.numOutcomesUnblinded AS covidNumOutcomesUnblinded,
                                    t.personYears AS covidPersonYears,
                                    t.incidenceRateP100py AS covidIncidenceRateP100py,
                                    o.numPersonsAtRisk*1.0/gs.numPersonsAtRiskTotal * t.incidenceRateP100PY AS covidWeightedIR,
                                    o.incidenceRateP100PY * t.personYears / 100.0 AS Ek
                                  FROM generalDF o
                                    JOIN generalGroupSummary gs
                                      ON gs.databaseName = o.databaseName
                                      AND gs.timeAtRiskId = o.timeAtRiskId
                                      AND gs.outcomeCohortDefinitionId = o.outcomeCohortDefinitionId
                                    JOIN covidDF t
                                      ON t.databaseName = o.databaseName
                                      AND t.timeAtRiskId = o.timeAtRiskId
                                      AND t.outcomeCohortDefinitionId = o.outcomeCohortDefinitionId
                                      AND t.subgroupCohortDefinitionId = o.subgroupCohortDefinitionId
                                      ")

  summaryIR <- setNames(aggregate(cbind(generalCovidDF$generalWeightedIR, generalCovidDF$covidWeightedIR, generalCovidDF$Ek, generalCovidDF$covidNumOutcomesUnblinded),
                                  list(generalCovidDF$databaseName, generalCovidDF$timeAtRiskId, generalCovidDF$outcomeName),
                                  FUN=sum),
                        c("databaseName", "timeAtRiskId", "outcomeName", "generalWeightedIRTotal", "covidWeightedIRTotal", "covidE", "covidD"))

  summaryIR$SIR <- summaryIR$covidD/summaryIR$covidE
  summaryIR$SIRLB = qchisq(p=0.025, df=2*summaryIR$covidD)/(2*summaryIR$covidE)
  summaryIR$SIRUB = qchisq(p=0.975, df=2*(summaryIR$covidD+1))/(2*summaryIR$covidE)

  summaryIR$logSIR <- log(summaryIR$SIR)
  summaryIR$logSIRSE = (log(summaryIR$SIRUB) - log(summaryIR$SIRLB ))  / 3.92

  #for each outcome, do meta-analysis over databases:
  outcomeName <- unique(summaryIR$outcomeName)
  metaResults <- data.frame(outcomeName)
  metaResults$SIR <- 0
  metaResults$SIRLB <- 0
  metaResults$SIRUB <- 0
  metaResults$SIRpredLB <- 0
  metaResults$SIRpredUB <- 0
  metaResults$I2 <- 0
  metaResults$numStudies <- 0


  for (i in 1:length(outcomeName)) {
    outcomeSubset <- summaryIR[summaryIR$outcomeName == outcomeName[i] & summaryIR$covidE > 1, ]
    outcomeMeta <- meta::metagen(data = outcomeSubset, studlab = databaseName, TE=logSIR, seTE = logSIRSE, sm="RR", method.tau = "DL")
    metaResults[i,"outcomeName"] <- outcomeName[i]
    metaResults[i,"SIR"] <- exp(outcomeMeta$TE.random)
    metaResults[i, "SIRLB"] <- exp(outcomeMeta$lower.random)
    metaResults[i, "SIRUB"] <- exp(outcomeMeta$upper.random)
    metaResults[i, "SIRpredLB"] <- exp(outcomeMeta$lower.predict)
    metaResults[i, "SIRpredUB"] <- exp(outcomeMeta$upper.predict)
    metaResults[i, "I2"] <- outcomeMeta$I2
    metaResults[i, "numStudies"] <- outcomeMeta$k
  }

  write.csv(summaryIR, paste0(resultsFolder ,"/summaryIR.csv"), row.names = FALSE)
  write.csv(metaResults, paste0(resultsFolder ,"/metaResults.csv"), row.names = FALSE)

}
