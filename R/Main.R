# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Covid19SubjectsAesiIncidenceRate
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
execute <- function(connectionDetails,
                    outputFolder,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable = paste0(databaseId, "_cohort"),
                    databaseId,
                    verifyDependencies = TRUE,
                    createCohortsAndRef = TRUE,
                    runCohortDiagnostics = TRUE,
                    runIR = TRUE){

  ################################
  # Setup
  ################################
  start <- Sys.time()

  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::clearLoggers()  # Ensure that any/all previous logging activities are cleared
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder,
                                                 paste0(getThisPackageName(), "_log.txt")))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, paste0(getThisPackageName(),
                                                                             "_ErrorReportR.txt")))
  ParallelLogger::addDefaultConsoleLogger()
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_CONSOLE_LOGGER", silent = TRUE), add = TRUE)

  # Write out the system information
  ParallelLogger::logInfo(.systemInfo())

  if (verifyDependencies) {
    ParallelLogger::logInfo("Checking whether correct package versions are installed")
    verifyDependencies()
  }

  #Variables---------------------
  tempEmulationSchema <- getOption("sqlRenderTempEmulationSchema")
  minCellCount= 5
  incrementalFolder = file.path(outputFolder, "incrementalFolder")

  ################################
  # STEP 1 - Create Cohorts
  ################################
  if(createCohortsAndRef){
    ParallelLogger::logInfo("**********************************************************")
    ParallelLogger::logInfo("  ---- Creating exposure and outcome cohorts ---- ")
    ParallelLogger::logInfo("**********************************************************")
    createCohortsAndRef(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        outputFolder = outputFolder,
                        incremental = TRUE,
                        cohortTablePrefix = databaseId)
  }

  ################################
  # STEP 2 - Run Cohort Diagnostics
  ################################
  if(runCohortDiagnostics){
    ParallelLogger::logInfo("**********************************************************")
    ParallelLogger::logInfo("  ---- Running cohort diagnostics ---- ")
    ParallelLogger::logInfo("**********************************************************")
    exportFolder <- file.path(outputFolder, "cohortDiagnostics")
    CohortDiagnostics::runCohortDiagnostics(packageName = "Covid19SubjectsAesiIncidenceRate",
                                            cohortToCreateFile = "settings/CohortsToCreate.csv",
                                            exportFolder = exportFolder,
                                            connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            cohortDatabaseSchema = cohortDatabaseSchema,
                                            cohortTable = cohortTable,
                                            tempEmulationSchema = tempEmulationSchema,
                                            databaseId = databaseId,
                                            databaseName = databaseName,
                                            databaseDescription = databaseDescription,
                                            runInclusionStatistics = TRUE,
                                            runIncludedSourceConcepts = TRUE,
                                            runOrphanConcepts = TRUE,
                                            runTimeDistributions = TRUE,
                                            runBreakdownIndexEvents = TRUE,
                                            runIncidenceRate = TRUE,
                                            runCohortOverlap = TRUE,
                                            runVisitContext = TRUE,
                                            runCohortCharacterization = TRUE,
                                            runTemporalCohortCharacterization = TRUE,
                                            runTimeSeries = FALSE,
                                            minCellCount = 5,
                                            incremental = TRUE,
                                            incrementalFolder = incrementalFolder)
    CohortDiagnostics::preMergeDiagnosticsFiles(exportFolder)
  }

  ################################
  # STEP 3 - Run Incidence Rate Analysis
  ################################
  if(runIR){
    ParallelLogger::logInfo("**********************************************************")
    ParallelLogger::logInfo("  ---- Running incidence rates ---- ")
    ParallelLogger::logInfo("**********************************************************")
    exportFolder <- file.path(outputFolder, "incidenceRate")
    runIR(connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTablePrefix = databaseId,
          exportFolder = exportFolder,
          databaseId = databaseId,
          databaseName = databaseName,
          databaseDescription = databaseDescription,
          incremental = TRUE)

  }

}
