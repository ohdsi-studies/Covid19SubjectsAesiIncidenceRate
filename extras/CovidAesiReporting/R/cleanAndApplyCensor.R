#'
#' @export
#'
#'
cleanAndApplyCensor <- function(irDf, notCensoredOutomesDF, censorSubgroupCohortDefinitionId, dataFolder,outcomeSortOrder){
  # NAME CLEANUP
  irDf[irDf == "Integrated Primary Care Information"] <- "IPCI"
  irDf[irDf == "UK Biobank, University College London"] <- "UK_BIOBANK"
  irDf[irDf == "University of Colorado - Health Data Compass"] <- "CU_AMC"
  irDf[irDf == "SIDIAP"] <- "SIDIAP"
  irDf[irDf == "University of Dundee HIC (Health Informatics Centre)"] <- "HIC"
  irDf[irDf == "IQVIA OpenClaims"] <- "IQVIA_OPENCLAIMS"
  irDf[irDf == "Stanford STARR OMOP"] <- "STARR"
  irDf[irDf == "Medaman Hospital Database"] <- "MHD"
  irDf[irDf == "OMOP FIIBAP CDM"] <- "FIIBAP"
  irDf[irDf == "EHR of Istanbul Faculty of Medicine, Istanbul University"] <- "IU"
  irDf[irDf == "Data Warehouse for Assisance Publique - Hôpitaux de Marseille"] <- "APHM"
  irDf[irDf == "University Clinical Center of Serbia - Heliant Health"] <- "UCCS"
  irDf[irDf == "University of Tartu"] <- "U_OF_TARTU"
  irDf[irDf == "CUIMC OMOP Database"] <- "CUIMC"
  irDf[irDf == "Parc de Salut Mar Barcelona Information System"] <- "IMASIS"
  irDf[irDf == "IQVIA(R) Disease Analyzer (DA) France"] <- "IQVIA_FRANCE_DA"
  irDf[irDf == "IQVIA(R) Disease Analyzer (DA) Germany"] <- "IQVIA_GERMANY_DA"
  irDf[irDf == "IQVIA(R) Australia Longitudinal Patient Data (LPD)"] <- "IQVIA_AUSTRALIA_LPD"
  irDf[irDf == "he IQVIA¨ Adjudicated Health Plan Claims Data"] <- "IQVIA_PHARMETRICS"
  irDf[irDf == "Japan Medical Data Center"] <- "JMDC"
  irDf[irDf == "IBM(R) MarketScan(R) Commercial Claims and Encounters Database"] <- "IBM_CCAE"
  irDf[irDf == "IBM MarketScan Medicare Supplemental and Coordination of Benefits Database"] <- "IBM_MDCR"
  irDf[irDf == "IBM(R) MarketScan(R) Multi-State Medicaid Database"] <- "IBM_MDCD"
  irDf[irDf == "Optum De-Identified Clinformatics(R) Data Mart Database -“ Socio-Economic Status (SES)"] <- "OPTUM_SES"
  irDf[irDf == "Optum(R) de-identified Electronic Health Record Dataset"] <- "OPTUM_EHR"
  irDf[irDf == "University of California Health"] <- "UCHDW"
  irDf[irDf == "CPRD_AURUM_UK"] <- "CPRD_AURUM"

  #saveIrDf <- irDf
  #irDf <- saveIrDf

  #Apply Censorship
  censoredIrDf <- sqldf::sqldf('SELECT i.*
              FROM irDf i
                LEFT OUTER JOIN notCensoredOutomesDF c
                  ON c.DB = i.databaseName
                  AND c.PHENOTYPE = i.outcomeId
              WHERE c.DB IS NOT NULL')

  censoredIrDf <- sqldf::sqldf('SELECT c.*
                     FROM censoredIrDf c
                      LEFT OUTER JOIN censorSubgroupCohortDefinitionId s
                        ON s.DB = c.databaseName
                        AND s.subgroupCohortDefinitionId = c.subgroupCohortDefinitionId
                     WHERE s.DB IS NULL')

  #More Cleanup
  censoredIrDf$gender <- ifelse(grepl("Female",censoredIrDf$subgroupName),"F",
                                ifelse(grepl("Male",censoredIrDf$subgroupName),"M",
                                       ifelse(grepl("All",censoredIrDf$subgroupName),"M&F",'-')))

  censoredIrDf$ageGroup <- ifelse(grepl("0 to 5", censoredIrDf$subgroupName),"000-005",
                                  ifelse(grepl("6 to 17", censoredIrDf$subgroupName),"006-017",
                                         ifelse(grepl("18 to 34",censoredIrDf$subgroupName),"018-034",
                                                ifelse(grepl("35 to 54",censoredIrDf$subgroupName),"035-054",
                                                       ifelse(grepl("55 to 64",censoredIrDf$subgroupName),"055-064",
                                                              ifelse(grepl("65 to 74",censoredIrDf$subgroupName),"065-074",
                                                                     ifelse(grepl("75 to 84",censoredIrDf$subgroupName),"075-084",
                                                                            ifelse(grepl("85 to 114",censoredIrDf$subgroupName),"085-114",
                                                                                   ifelse(grepl("0 to 4",   censoredIrDf$subgroupName),"000-004",
                                                                                          ifelse(grepl("5 to 11", censoredIrDf$subgroupName),"005-011",
                                                                                                 ifelse(grepl("12 to 17",censoredIrDf$subgroupName),"012-017",'ALL')))))))))))

  #Streamline multi-definitions
  censoredIrDf[censoredIrDf == "Anaphylaxis v2.0"] <- "Anaphylaxis"
  censoredIrDf$outcomeId[censoredIrDf$outcomeId==568] <- 349
  censoredIrDf$outcomeCohortDefinitionId[censoredIrDf$outcomeCohortDefinitionId==568] <- 349

  #Some DPs had duplicate records
  censoredIrDf <- unique(censoredIrDf)

  #Add sort order to the file
  censoredIrDf <- merge(x = censoredIrDf, y = outcomeSortOrder, by = 'outcomeName', all.x = TRUE)

  censoredIrDfCovid <- censoredIrDf[censoredIrDf$targetName =="Earliest COVID-19 Event (positive test OR diagnosis)",]
  censoredIrDfGeneral <- censoredIrDf[censoredIrDf$targetName =="Persons at Risk at Start of Year 2017-2019",]
  censoredIrDfCovidGeneral <- sqldf::sqldf("SELECT DISTINCT c.*, g.incidenceRateP100py As incidenceRateP100pyGeneral
                                         FROM censoredIrDfCovid c
                                          JOIN censoredIrDfGeneral g
                                            ON g.databaseName = c.databaseName
                                            AND g.timeAtRiskId = c.timeAtRiskId
                                            AND g.subgroupCohortDefinitionId = c.subgroupCohortDefinitionId
                                            AND g.outcomeId = c.outcomeId
                                            AND g.ageGroup = c.ageGroup")

  write.csv(censoredIrDfCovidGeneral,paste0(dataFolder,"/incidenceAnalysisCensoredCovidVsGeneral.csv"), row.names = FALSE)

  return(censoredIrDf)

}
