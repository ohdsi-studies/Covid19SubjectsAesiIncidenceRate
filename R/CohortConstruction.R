# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of Covid19SubjectsAesiIncidenceRate19VaccineAesiIncidenceRate19VaccineAesiIncidenceCharacterization
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

#' Create cohort table(s)
#'
#' @description
#' This function creates an empty cohort table. Optionally, additional empty tables are created to
#' store statistics on the various inclusion criteria.
#'
#' @template Connection
#'
#' @template CohortTable
#'
#' @param createInclusionStatsTables   Create the four additional tables for storing inclusion rule
#'                                     statistics?
#' @param resultsDatabaseSchema        Schema name where the statistics tables reside. Note that for
#'                                     SQL Server, this should include both the database and schema
#'                                     name, for example 'scratch.dbo'.
#' @param cohortInclusionTable         Name of the inclusion table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortInclusionResultTable   Name of the inclusion result table, one of the tables for
#'                                     storing inclusion rule statistics.
#' @param cohortInclusionStatsTable    Name of the inclusion stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortSummaryStatsTable      Name of the summary stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#'
#' @export
createCohortTable <- function(connectionDetails = NULL,
                              connection = NULL,
                              cohortDatabaseSchema,
                              tempEmulationSchema = NULL,
                              cohortTable = "cohort") {
  start <- Sys.time()
  ParallelLogger::logInfo("Creating cohort table")
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  sql <- SqlRender::loadRenderTranslateSql("CreateCohortTable.sql",
                                           packageName = getThisPackageName(),

    dbms = connection@dbms, cohort_database_schema = cohortDatabaseSchema, tempEmulationSchema = tempEmulationSchema,
    cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  ParallelLogger::logDebug("- Created table ", cohortDatabaseSchema, ".", cohortTable)
  delta <- Sys.time() - start
  writeLines(paste("Creating cohort table took", signif(delta, 3), attr(delta, "units")))
}


#' Instantiate a set of cohort
#'
#' @description
#' This function instantiates a set of cohort in the cohort table, using definitions that are fetched
#' from a WebApi interface. Optionally, the inclusion rule statistics are computed and stored in the
#' \code{inclusionStatisticsFolder}.
#'
#' @template Connection
#'
#' @template CohortTable
#'
#' @template TempEmulationSchema
#'
#' @template CdmDatabaseSchema
#'
#' @template CohortSetSpecs
#'
#' @template CohortSetReference
#'
#' @param cohortIds                   Optionally, provide a subset of cohort IDs to restrict the
#'                                    construction to.
#' @param generateInclusionStats      Compute and store inclusion rule statistics?
#' @param inclusionStatisticsFolder   The folder where the inclusion rule statistics are stored. Can be
#'                                    left NULL if \code{generateInclusionStats = FALSE}.
#' @param createCohortTable           Create the cohort table? If \code{incremental = TRUE} and the
#'                                    table already exists this will be skipped.
#' @param incremental                 Create only cohorts that haven't been created before?
#' @param incrementalFolder           If \code{incremental = TRUE}, specify a folder where records are
#'                                    kept of which definition has been executed.
#'
#' @export
instantiateCohortSet <- function(connectionDetails = NULL,
                                 connection = NULL,
                                 cdmDatabaseSchema,
                                 tempEmulationSchema = NULL,
                                 cohortDatabaseSchema = cdmDatabaseSchema,
                                 cohortTable = "cohort",
                                 cohorts = NULL,
                                 cohortSqlFolder = "",
                                 createCohortTable = FALSE,
                                 incremental = FALSE,
                                 incrementalFolder = NULL,
                                 ...) {
  if (incremental) {
    if (is.null(incrementalFolder)) {
      stop("Must specify incrementalFolder when incremental = TRUE")
    }
    if (!file.exists(incrementalFolder)) {
      dir.create(incrementalFolder, recursive = TRUE)
    }
  }

  start <- Sys.time()
  if (is.null(connection)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))
  }
  if (createCohortTable) {
    needToCreate <- TRUE
    if (incremental) {
      tables <- DatabaseConnector::getTableNames(connection, cohortDatabaseSchema)
      if (toupper(cohortTable) %in% toupper(tables)) {
        ParallelLogger::logInfo("Cohort table already exists and in incremental mode, so not recreating table.")
        needToCreate <- FALSE
      }
    }
    if (needToCreate) {
      createCohortTable(connection = connection,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable)
    }
  }

  # cohorts <- loadCohortsFromPackage(cohortIds = cohortIds)
  getSql <- function(sqlFileName, cohortSqlFolder) {
    pathToSql <- system.file("sql",
                             "sql_server",
                             cohortSqlFolder,
                             sqlFileName,
                             package = getThisPackageName(),

                             mustWork = TRUE)
    sql <- readChar(pathToSql, file.info(pathToSql)$size)
    return(sql)
  }
  cohorts$sql <- sapply(cohorts$fileName, getSql, cohortSqlFolder)

  if (incremental) {
    cohorts$checksum <- computeChecksum(cohorts$sql)
    recordKeepingFile <- file.path(incrementalFolder, "InstantiatedCohorts.csv")
  }

  instantiatedCohortIds <- c()
  for (i in 1:nrow(cohorts)) {
    if (!incremental || isTaskRequired(cohortId = cohorts$cohortId[i],
                                       checksum = cohorts$checksum[i],

                                       recordKeepingFile = recordKeepingFile)) {
      ParallelLogger::logInfo(i,
                              "/",
                              nrow(cohorts),
                              ": Instantiation cohort ",
                              cohorts$cohortName[i],

                              "  (", cohorts$fileName[i], ")")
      sql <- cohorts$sql[i]
      sql <- SqlRender::render(sql,
                               cdm_database_schema = cdmDatabaseSchema,
                               vocabulary_database_schema = cdmDatabaseSchema,

                               target_database_schema = cohortDatabaseSchema, target_cohort_table = cohortTable, target_cohort_id = cohorts$cohortId[i],
                               ..., warnOnMissingParameters = FALSE)
      sql <- SqlRender::translate(sql,
                                  targetDialect = connectionDetails$dbms,
                                  tempEmulationSchema = tempEmulationSchema)
      DatabaseConnector::executeSql(connection, sql)
      instantiatedCohortIds <- c(instantiatedCohortIds, cohorts$cohortId[i])
      if (incremental) {
        recordTasksDone(cohortId = cohorts$cohortId[i],
                        checksum = cohorts$checksum[i],
                        recordKeepingFile = recordKeepingFile)
      }
    }
  }

  delta <- Sys.time() - start
  writeLines(paste("Instantiating cohort set took", signif(delta, 3), attr(delta, "units")))
  return(instantiatedCohortIds)
}
