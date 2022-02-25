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

# CREATE LOCK FILE --------------------------------------------------------
library(keyring)
library(Covid19SubjectsAesiIncidenceRate)
renv::deactivate()
remove.packages("Covid19SubjectsAesiIncidenceRate")
renv::purge("Covid19SubjectsAesiIncidenceRate")
OhdsiRTools::createRenvLockFile(rootPackage = "Covid19SubjectsAesiIncidenceRate",
                                additionalRequiredPackages = c("keyring", "Covid19SubjectsAesiIncidenceRate"),
                                includeRootPackage = TRUE)
#renv::update(packages = c("CohortDiagnostics"), library = "C:\\Users\\admin_evoss3\\Documents\\R\\win-library\\4.1")
renv::restore()
renv::restore(packages = "CohortDiagnostics")

# Format and check code ---------------------------------------------------
# OhdsiRTools::formatRFolder()
# OhdsiRTools::updatePackageNameFolder(packageName = "Covid19SubjectsAesiIncidenceRate",
#                                      recursive = T)
# OhdsiRTools::checkUsagePackage("Covid19SubjectsAesiIncidenceRate")
# OhdsiRTools::updateCopyrightYearFolder()
# devtools::document()
# Sys.setenv(JAVA_HOME = "")
# devtools::check()

# Create manual -----------------------------------------------------------
# unlink("extras/Covid19SubjectsAesiIncidenceRate.pdf")
# shell("R CMD Rd2pdf ./ --output=extras/Covid19SubjectsAesiIncidenceRate.pdf")
#
# pkgdown::build_site()

# CREATE ANALYSIS.json
analysis1 <- list(name = "AESIs in COVID-19 subjects",
                  targetIds = list(562, 563, 565, 566),
                  subgroupIds = list(21,22,31,32,41,42,51,52,61,62,71,72,81,82,91,92,101,102,111,112,121,122),
                  timeAtRiskIds = list(1, 2, 3, 4, 5, 6, 7),
                  outcomeIds = list(345, 349, 568, 347, 346, 343, 340, 339, 335, 385, 386, 381, 405, 402, 406, 411, 547))

analysisList <- list(analysisList = list(analysis1))
analysisListJson <- RJSONIO::toJSON(analysisList, pretty = T)
analysisListFile <- "D:/Git/GitHub/Covid19SubjectsAesiIncidenceRate/inst/settings/analysisSettings.json"
write(analysisListJson, file = analysisListFile)

# Verify the analysis settings ---------------
targetRef <- system.file("settings/targetRef.csv",
                         package = "Covid19SubjectsAesiIncidenceRate",
                         mustWork = TRUE)
subgroupRef <- system.file("settings/subgroupRef.csv", package = "Covid19SubjectsAesiIncidenceRate",
  mustWork = TRUE)
outcomeRef <- system.file("settings/outcomeRef.csv",
                          package = "Covid19SubjectsAesiIncidenceRate",
                          mustWork = TRUE)
tarRef <- system.file("settings/timeAtRisk.csv",
                      package = "Covid19SubjectsAesiIncidenceRate",
                      mustWork = TRUE)

targetCohort <- readr::read_csv(targetRef, col_types = readr::cols())
subgroupCohorts <- readr::read_csv(subgroupRef, col_types = readr::cols())
outcomeCohorts <- readr::read_csv(outcomeRef, col_types = readr::cols())
tars <- readr::read_csv(tarRef, col_types = readr::cols())

targetIds <- targetCohort$cohortId
subgroupIds <- subgroupCohorts$cohortId
outcomeIds <- outcomeCohorts$outcomeId
tarIds <- tars$time_at_risk_id

analysisListJsonFromFile <- paste(readLines(analysisListFile), collapse = "\n")
analysisListFromFile <- RJSONIO::fromJSON(analysisListJsonFromFile)
compareLists <- function(x, y) {
  xCompareY <- x %in% y
  xInY <- which(xCompareY)
  return(length(x) == length(xInY))
}

for (i in 1:length(analysisListFromFile$analysisList)) {
  print(paste(i, analysisListFromFile$analysisList[[i]]$name))
  if (!(compareLists(analysisListFromFile$analysisList[[i]]$targetIds, targetIds))) {
    warning("Target ID mismatch from targetRef.csv file.")
  }
  # These are generated dynamically by runIncidenceAnalysis.sql so we can't check these at design
  # time.  if (!(compareLists(analysisListFromFile$analysisList[[i]]$subgroupIds, subgroupIds))) {
  # warning('Subgroup ID mismatch from subgroupRef.csv file.') }
  if (!(compareLists(analysisListFromFile$analysisList[[i]]$outcomeIds, outcomeIds))) {
    warning("Outcome ID mismatch from outcomeRef.csv file.")
  }
  if (!(compareLists(analysisListFromFile$analysisList[[i]]$timeAtRiskIds, tarIds))) {
    warning("Time-at-risk ID mismatch from timeAtRisk.csv file.")
  }
}

# Check the warnings if any are generated to find configuration errors


# # Store environment in which the study was executed -----------------------
# OhdsiRTools::insertEnvironmentSnapshotInPackage("Covid19SubjectsAesiIncidenceRate")
# OhdsiRTools::createRenvLockFile(rootPackage = "Covid19SubjectsAesiIncidenceRate")
#
# # Check all files for UTF-8 Encoding and ensure there are no non-ASCII characters
# OhdsiRTools::findNonAsciiStringsInFolder()
#
# packageFiles <- list.files(path = ".", recursive = TRUE)
# if (!all(utf8::utf8_valid(packageFiles))) {
#   print("Found invalid UTF-8 encoded files")
# }
#
# # Create the Renv lock file
# OhdsiRTools::createRenvLockFile("Covid19SubjectsAesiIncidenceRate",
#                                 additionalRequiredPackages = c("keyring",
#   "DatabaseConnector", "dplyr", "ROhdsiWebApi", "stringr", "SqlRender", "tidyr", "plyr"))
#
# # Validate cohort SQL file names ------------
# targetCohorts <- Covid19SubjectsAesiIncidenceRate::readCsv("settings/targetRef.csv")
# targetCohorts$cohortFolder <- "target"
# subgroupCohorts <- Covid19SubjectsAesiIncidenceRate::readCsv("settings/subgroupRef.csv")
# subgroupCohorts$cohortFolder <- "subgroup"
# outcomeCohorts <- Covid19SubjectsAesiIncidenceRate::readCsv("settings/outcomeRef.csv")
# outcomeCohorts$cohortFolder <- "outcome"
# # Reformat the outcomeCohorts dataframe to match target/subgroup
# outcomeCohortsReformatted <- outcomeCohorts[, c("outcomeId",
#                                                 "outcomeName",
#                                                 "fileName",
#                                                 "cohortFolder")]
# names(outcomeCohortsReformatted) <- c("cohortId", "cohortName", "fileName", "cohortFolder")
# allCohorts <- rbind(targetCohorts, subgroupCohorts, outcomeCohortsReformatted)
#
# # Obtain the list of SQL files in the list
# packageSqlFiles <- list.files(system.file(file.path("sql/sql_server/"),
#                                           package = "Covid19SubjectsAesiIncidenceRate"),
#   recursive = TRUE)
#
# for (i in 1:nrow(allCohorts)) {
#   # Verify that the path to the SQL file is correct and matches with case sensitivity
#   sqlFileName <- file.path(allCohorts$cohortFolder[i], allCohorts$fileName[i])
#   fileFound <- sqlFileName %in% packageSqlFiles
#   if (!fileFound) {
#     warning(paste(sqlFileName,
#                   "not found in package. This is likely due to a difference in case sensitivity."))
#   }
# }
