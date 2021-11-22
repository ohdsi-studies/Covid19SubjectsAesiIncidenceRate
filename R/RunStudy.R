# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Covid19SubjectsAesiIncidenceRate19SubjectsAesiIncidenceRate
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#'
#' @export
runIR <- function(connectionDetails = NULL,
                     connection = NULL,
                     cdmDatabaseSchema,
                     tempEmulationSchema = NULL,
                     cohortDatabaseSchema,
                     cohortTablePrefix = "aesi",
                     targetCohortTable = paste0(cohortTablePrefix, "_target"),
                     targetRefTable = paste0(cohortTablePrefix, "_target_ref"),
                     subgroupCohortTable = paste0(cohortTablePrefix, "_subgroup"),
                     subgroupRefTable = paste0(cohortTablePrefix, "_subgroup_ref"),
                     outcomeCohortTable = paste0(cohortTablePrefix, "_outcome"),
                     outcomeRefTable = paste0(cohortTablePrefix, "_outcome_ref"),
                     timeAtRiskTable = paste0(cohortTablePrefix, "_time_at_risk"),
                     summaryTable = paste0(cohortTablePrefix, "_ir_summary"),
                     exportFolder,
                     databaseId,
                     databaseName = databaseId,
                     databaseDescription = "",
                     minCellCount = 5,
                     incremental = TRUE,
                     incrementalFolder = file.path(exportFolder, "RecordKeeping")) {

  start <- Sys.time()

  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }


  if (incremental) {
    if (is.null(incrementalFolder)) {
      stop("Must specify incrementalFolder when incremental = TRUE")
    }
    if (!file.exists(incrementalFolder)) {
      dir.create(incrementalFolder, recursive = TRUE)
    }
  }

  if (!is.null(getOption("andromedaTempFolder")) && !file.exists(getOption("andromedaTempFolder"))) {
    warning("andromedaTempFolder '",
            getOption("andromedaTempFolder"),
            "' not found. Attempting to create folder")
    dir.create(getOption("andromedaTempFolder"), recursive = TRUE)
  }

  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }

  # Run the IR analysis ------------------------------------------------------------
  ParallelLogger::logInfo("----------------------------------------------------------")
  ParallelLogger::logInfo("  ---- Computing & Export Incidence Analysis ---- ")
  ParallelLogger::logInfo("----------------------------------------------------------")
  computeAndExportIncidenceAnalysis(connection,
                                    exportFolder,
                                    tempEmulationSchema,
                                    cdmDatabaseSchema,
                                    cohortDatabaseSchema,
                                    targetCohortTable,
                                    targetRefTable,
                                    subgroupCohortTable,
                                    subgroupRefTable,
                                    outcomeCohortTable,
                                    outcomeRefTable,
                                    timeAtRiskTable,
                                    databaseName,
                                    summaryTable,
                                    minCellCount)


  # Save database metadata ---------------------------------------------------------------
  ParallelLogger::logInfo("Saving database metadata")
  op <- getObservationPeriodDateRange(connection,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      tempEmulationSchema = tempEmulationSchema)
  vocabInfo <- getVocabularyInfo(connection = connection,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 tempEmulationSchema = tempEmulationSchema)
  vocabularyVersion <- "UNKNOWN"
  if (nrow(vocabInfo) >= 1) {
    vocabularyVersion <- vocabInfo[[1]]
  }
  database <- data.frame(databaseId = databaseId,
                         databaseName = databaseName,
                         description = databaseDescription,
                         vocabularyVersion = vocabularyVersion,
                         minObsPeriodDate = op$minObsPeriodDate,
                         maxObsPeriodDate = op$maxObsPeriodDate,
                         isMetaAnalysis = 0)
  writeToCsv(database, file.path(exportFolder, "database.csv"))

  # Save package metadata ---------------------------------------------------------------
  ParallelLogger::logInfo("Saving package metadata")
  packageVersionNumber <- packageVersion(getThisPackageName())
  packageMetadata <- data.frame(packageId = getThisPackageName(),
                                packageVersion = packageVersionNumber,
                                executionDate = start,
                                params = RJSONIO::toJSON(list(targetCohortTable = targetCohortTable,
                                                              targetRefTable = targetRefTable,
                                                              subgroupCohortTable = subgroupCohortTable,
                                                              subgroupRefTable = subgroupRefTable,
                                                              outcomeCohortTable = outcomeCohortTable,
                                                              outcomeRefTable = outcomeRefTable,
                                                              timeAtRiskTable = timeAtRiskTable,
                                                              summaryTable = summaryTable,
                                                              exportFolder = exportFolder,
                                                              databaseId = databaseId,
                                                              databaseName = databaseName,
                                                              databaseDescription = databaseDescription,
                                                              minCellCount = minCellCount,
                                                              incremental = incremental,
                                                              incrementalFolder = incrementalFolder)))
  writeToCsv(packageMetadata, file.path(exportFolder, "package.csv"))


  # Export to zip file -------------------------------------------------------------------------------
  zipName <- zipResults(exportFolder, databaseId)
  ParallelLogger::logInfo("Results are ready for sharing at:", zipName)

  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Running study took", round(delta, 2), attr(delta, "units")))
}

computeAndExportIncidenceAnalysis <- function(connection,
                                              exportFolder,
                                              tempEmulationSchema,
                                              cdmDatabaseSchema,
                                              cohortDatabaseSchema,
                                              targetCohortTable,
                                              targetRefTable,
                                              subgroupCohortTable,
                                              subgroupRefTable,
                                              outcomeCohortTable,
                                              outcomeRefTable,
                                              timeAtRiskTable,
                                              databaseName,
                                              summaryTable,
                                              minCellCount) {

  # Run the analyses as specified in the settings/analysisSettings.json file
  analysisListFile <- system.file("settings/analysisSettings.json",
                                  package = getThisPackageName(),
                                  mustWork = TRUE)
  analysisListJsonFromFile <- paste(readLines(analysisListFile), collapse = "\n")
  analysisList <- RJSONIO::fromJSON(analysisListJsonFromFile)


  for (i in 1:length(analysisList$analysisList)) {
    ParallelLogger::logInfo(paste0("(",
                                   i,
                                   "/",
                                   length(analysisList$analysisList),
                                   "): ",
                                   analysisList$analysisList[[i]]$name))
    targetIds <- analysisList$analysisList[[i]]$targetIds
    subgroupIds <- analysisList$analysisList[[i]]$subgroupIds
    timeAtRiskIds <- analysisList$analysisList[[i]]$timeAtRiskIds
    outcomeIds <- analysisList$analysisList[[i]]$outcomeIds


    runIncidenceAnalysisSql <- SqlRender::loadRenderTranslateSql("runIncidenceAnalysis.sql",
                                                                 packageName = getThisPackageName(),
                                                                 dbms = connection@dbms,
                                                                 tempEmulationSchema = tempEmulationSchema,
                                                                 warnOnMissingParameters = TRUE,
                                                                 cdm_database_schema = cdmDatabaseSchema,
                                                                 cohort_database_schema = cohortDatabaseSchema,
                                                                 target_cohort_table = targetCohortTable,
                                                                 target_ref_table = targetRefTable,
                                                                 target_ids = targetIds,
                                                                 subgroup_cohort_table = subgroupCohortTable,
                                                                 subgroup_ref_table = subgroupRefTable,
                                                                 subgroup_ids = subgroupIds,
                                                                 outcome_cohort_table = outcomeCohortTable,
                                                                 outcome_ref_table = outcomeRefTable,
                                                                 outcome_ids = outcomeIds,
                                                                 time_at_risk_table = timeAtRiskTable,
                                                                 time_at_risk_ids = timeAtRiskIds,
                                                                 database_name = databaseName,
                                                                 summary_table = summaryTable)
    DatabaseConnector::executeSql(connection = connection,
                                  sql = runIncidenceAnalysisSql,
                                  progressBar = TRUE,
                                  reportOverallTime = TRUE)
  }

  # Export & Censor results
  ParallelLogger::logInfo("Exporting analysis results")
  getIncidenceAnalysisSql <- SqlRender::loadRenderTranslateSql("GetIncidenceAnalysisResults.sql",
                                                               packageName = getThisPackageName(),
                                                               dbms = connection@dbms,
                                                               tempEmulationSchema = tempEmulationSchema,
                                                               warnOnMissingParameters = TRUE,
                                                               cohort_database_schema = cohortDatabaseSchema,
                                                               summary_table = summaryTable)

  results <- DatabaseConnector::querySql(connection = connection,
                                         sql = getIncidenceAnalysisSql,
                                         snakeCaseToCamelCase = TRUE)

  # Censor the results
  ParallelLogger::logInfo("Censoring results")
  fieldsToCensor <- c("numPersonsWOutcomePreExclude",
                      "numPersonsWOutcome",
                      "numOutcomesPreExclude",
                      "numOutcomes")
  for (i in 1:length(fieldsToCensor)) {
    results <- enforceMinCellValue(results, fieldsToCensor[i], minCellCount)
  }

  writeToCsv(results, file.path(exportFolder, "incidence_analysis.csv"))
}

#' @export
insertRefEntries <- function(connection,
                             sqlFile,
                             cohortDatabaseSchema,
                             tableName,
                             tempEmulationSchema,
                             ...) {
  sql <- SqlRender::loadRenderTranslateSql(sqlFile,
                                           packageName = getThisPackageName(),
                                           dbms = connection@dbms,
                                           tempEmulationSchema = tempEmulationSchema,
                                           warnOnMissingParameters = TRUE,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           ref_table = tableName,
                                           ...)
  DatabaseConnector::executeSql(connection = connection,
                                sql = sql,
                                progressBar = FALSE,
                                reportOverallTime = FALSE)
}

zipResults <- function(exportFolder, databaseId) {
  zipName <- file.path(exportFolder, paste0("Results_IR_", databaseId, ".zip"))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  return(zipName)
}

getVocabularyInfo <- function(connection, cdmDatabaseSchema, tempEmulationSchema) {
  sql <- "SELECT vocabulary_version FROM @cdm_database_schema.vocabulary WHERE vocabulary_id = 'None';"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql,
                              targetDialect = attr(connection, "dbms"),
                              tempEmulationSchema = tempEmulationSchema)
  vocabInfo <- DatabaseConnector::querySql(connection, sql)
  return(vocabInfo)
}

getObservationPeriodDateRange <- function(connection, cdmDatabaseSchema, tempEmulationSchema) {
  sql <- "SELECT MIN(observation_period_start_date) min_obs_period_date, MAX(observation_period_end_date) max_obs_period_date FROM @cdm_database_schema.observation_period;"
  sql <- SqlRender::render(sql, cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql,
                              targetDialect = attr(connection, "dbms"),
                              tempEmulationSchema = tempEmulationSchema)
  op <- DatabaseConnector::querySql(connection, sql)
  names(op) <- SqlRender::snakeCaseToCamelCase(names(op))
  return(op)
}

writeToCsv <- function(data, fileName, incremental = FALSE, ...) {
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  if (incremental) {
    params <- list(...)
    names(params) <- SqlRender::camelCaseToSnakeCase(names(params))
    params$data <- data
    params$fileName <- fileName
    do.call(saveIncremental, params)
  } else {
    readr::write_csv(data, fileName)
  }
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}
