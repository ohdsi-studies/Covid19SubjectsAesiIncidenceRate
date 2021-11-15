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
createCohortsAndRef <- function(connectionDetails,
                                cdmDatabaseSchema,
                                vocabularyDatabaseSchema = cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTablePrefix = "AESI",
                                targetCohortTable = paste0(cohortTablePrefix, "_target"),
                                targetRefTable = paste0(cohortTablePrefix, "_target_ref"),
                                subgroupCohortTable = paste0(cohortTablePrefix, "_subgroup"),
                                subgroupRefTable = paste0(cohortTablePrefix, "_subgroup_ref"),
                                outcomeCohortTable = paste0(cohortTablePrefix, "_outcome"),
                                outcomeRefTable = paste0(cohortTablePrefix, "_outcome_ref"),
                                cohortTable = paste0(cohortTablePrefix, "_cohort"),
                                timeAtRiskTable = paste0(cohortTablePrefix, "_time_at_risk"),
                                summaryTable = paste0(cohortTablePrefix, "_ir_summary"),
                                outputFolder,
                                tempEmulationSchema = NULL,
                                incremental = TRUE,
                                incrementalFolder = file.path(outputFolder, "RecordKeeping")) {

  connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

  # Create the reference tables -----------------------------------------------------------------------
  ParallelLogger::logInfo("----------------------------------------------------------")
  ParallelLogger::logInfo("  ---- Creating reference tables  ---- ")
  ParallelLogger::logInfo("----------------------------------------------------------")
  createRefTablesSql <- SqlRender::loadRenderTranslateSql("CreateRefTables.sql",
                                                          packageName = getThisPackageName(),
                                                          dbms = connection@dbms,
                                                          tempEmulationSchema = tempEmulationSchema,
                                                          warnOnMissingParameters = TRUE,
                                                          cohort_database_schema = cohortDatabaseSchema,
                                                          summary_table = summaryTable,
                                                          target_ref_table = targetRefTable,
                                                          subgroup_ref_table = subgroupRefTable,
                                                          outcome_ref_table = outcomeRefTable,
                                                          time_at_risk_table = timeAtRiskTable)
  DatabaseConnector::executeSql(connection = connection,
                                sql = createRefTablesSql,
                                progressBar = TRUE,
                                reportOverallTime = TRUE)

  # Instantiate cohorts -----------------------------------------------------------------------
  targetCohorts <- readCsv("settings/targetRef.csv")
  subgroupCohorts <- readCsv("settings/subgroupRef.csv")
  outcomeCohorts <- readCsv("settings/outcomeRef.csv")
  timeAtRisk <- readCsv("settings/timeAtRisk.csv")

  instantiatedTargetCohortIds <- c()
  if (nrow(targetCohorts) > 0) {
    ParallelLogger::logInfo("----------------------------------------------------------")
    ParallelLogger::logInfo("  ---- Creating target cohorts ---- ")
    ParallelLogger::logInfo("----------------------------------------------------------")
    instantiateCohortSet(connectionDetails = connectionDetails,
                         connection = connection,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = targetCohortTable,
                         cohorts = targetCohorts,
                         cohortSqlFolder = "target",
                         createCohortTable = TRUE,
                         incremental = incremental,
                         incrementalFolder = incrementalFolder)

    # Create the ref table
    ParallelLogger::logInfo("Insert target reference")
    for (i in 1:nrow(targetCohorts)) {
      insertRefEntries(connection = connection,
                       sqlFile = "InsertTargetRef.sql",
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       tableName = targetRefTable,
                       tempEmulationSchema = tempEmulationSchema,
                       target_cohort_definition_id = targetCohorts$cohortId[i],
                       target_name = targetCohorts$cohortName[i])
    }

  }


  if (nrow(timeAtRisk) > 0) {
    ParallelLogger::logInfo("Insert time at risk reference")
    for (i in 1:nrow(timeAtRisk)) {
      insertRefEntries(connection = connection,
                       sqlFile = "InsertTimeAtRiskRef.sql",
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       tempEmulationSchema = tempEmulationSchema,
                       tableName = timeAtRiskTable,
                       time_at_risk_id = timeAtRisk$time_at_risk_id[i],
                       time_at_risk_start_offset = timeAtRisk$time_at_risk_start_offset[i],
                       time_at_risk_start_index = timeAtRisk$time_at_risk_start_index[i],
                       time_at_risk_end_offset = timeAtRisk$time_at_risk_end_offset[i],
                       time_at_risk_end_index = timeAtRisk$time_at_risk_end_index[i])
    }
  }

  if (nrow(subgroupCohorts) > 0) {
    ParallelLogger::logInfo("----------------------------------------------------------")
    ParallelLogger::logInfo("  ---- Creating subgroup cohorts ---- ")
    ParallelLogger::logInfo("----------------------------------------------------------")
    instantiateCohortSet(connectionDetails = connectionDetails,
                         connection = connection,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = subgroupCohortTable,
                         cohorts = subgroupCohorts,
                         cohortSqlFolder = "subgroup",
                         createCohortTable = TRUE,
                         incremental = incremental,
                         incrementalFolder = incrementalFolder,
                         target_ref_table = subgroupRefTable)  # NOTE: Extra param target_ref_table
  }

  if (nrow(outcomeCohorts) > 0) {
    # The outcome ref file is a little different from the others so this step aims to normalize it to the
    # other format of cohortId, cohortName, fileName
    outcomeCohortsToCreate <- outcomeCohorts[, c("outcomeId", "outcomeName", "fileName")]
    names(outcomeCohortsToCreate) <- c("cohortId", "cohortName", "fileName")

    ParallelLogger::logInfo("----------------------------------------------------------")
    ParallelLogger::logInfo("  ---- Creating outcome cohorts ---- ")
    ParallelLogger::logInfo("----------------------------------------------------------")
    instantiateCohortSet(connectionDetails = connectionDetails,
                         connection = connection,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         tempEmulationSchema = tempEmulationSchema,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = outcomeCohortTable,
                         cohorts = outcomeCohortsToCreate,
                         cohortSqlFolder = "outcome",
                         createCohortTable = TRUE,
                         incremental = incremental,
                         incrementalFolder = incrementalFolder)

    # Populate ref table
    ParallelLogger::logInfo("Insert outcome reference")
    for (i in 1:nrow(outcomeCohorts)) {
      insertRefEntries(connection = connection,
                       sqlFile = "InsertOutcomeRef.sql",
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       tempEmulationSchema = tempEmulationSchema,
                       tableName = outcomeRefTable,
                       outcome_id = outcomeCohorts$outcomeId[i],
                       outcome_cohort_definition_id = outcomeCohorts$outcomeCohortDefinitionId[i],
                       outcome_name = outcomeCohorts$outcomeName[i],
                       clean_window = outcomeCohorts$cleanWindow[i],
                       primary_time_at_risk_start_offset = outcomeCohorts$primaryTimeAtRiskStartOffset[i],
                       primary_time_at_risk_start_index = outcomeCohorts$primaryTimeAtRiskStartIndex[i],
                       primary_time_at_risk_end_offset = outcomeCohorts$primaryTimeAtRiskEndOffset[i],
                       primary_time_at_risk_end_index = outcomeCohorts$primaryTimeAtRiskEndIndex[i],
                       excluded_cohort_definition_id = outcomeCohorts$excludedCohortDefinitionId[i])
    }
  }

  if (nrow(targetCohorts) > 0 || nrow(outcomeCohorts) > 0){
    ParallelLogger::logInfo("----------------------------------------------------------")
    ParallelLogger::logInfo("  ---- Union Cohort Tables for Cohort Diagnostics ---- ")
    ParallelLogger::logInfo("----------------------------------------------------------")
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTableUnion.sql",
                                             packageName = "Covid19SubjectsAesiIncidenceRate",
                                             dbms = connection@dbms,
                                             cohort_database_schema = cohortDatabaseSchema,
                                             cohort_table = cohortTable,
                                             outcomeCohortTable = outcomeCohortTable,
                                             targetCohortTable = targetCohortTable)
    DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)

  }

  DatabaseConnector::disconnect(connection)
}

