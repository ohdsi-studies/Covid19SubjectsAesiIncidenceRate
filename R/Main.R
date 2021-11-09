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
                    cohortTable,
                    databaseId,
                    createCohorts = TRUE,
                    runCohortDiagnostics = TRUE){

  ################################
  # Setup
  ################################
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)

  #Variables---------------------
  tempEmulationSchema <- getOption("sqlRenderTempEmulationSchema")
  minCellCount= 5
  incrementalFolder = file.path(outputFolder, "incrementalFolder")

  ################################
  # STEP 1 - Create Cohorts
  ################################
  if(createCohorts){
    ParallelLogger::logInfo("Creating exposure and outcome cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  outputFolder = outputFolder)
  }

  ################################
  # STEP 2 - Run Cohort Diagnostics
  ################################
  if(runCohortDiagnostics){
    ParallelLogger::logInfo("Running cohort diagnostics")
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

}
