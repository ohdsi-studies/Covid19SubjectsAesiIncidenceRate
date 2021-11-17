# @file Logging.R
#
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
.systemInfo <- function() {
  si <- sessionInfo()
  lines <- c()
  lines <- c(lines, "R version:")
  lines <- c(lines, si$R.version$version.string)
  lines <- c(lines, "")
  lines <- c(lines, "Platform:")
  lines <- c(lines, si$R.version$platform)
  lines <- c(lines, "")
  lines <- c(lines, "Locale:")
  lines <- c(lines, si$locale)
  lines <- c(lines, "")
  lines <- c(lines, "Attached base packages:")
  lines <- c(lines, paste("-", si$basePkgs))
  lines <- c(lines, "")
  lines <- c(lines, "Other attached packages:")
  for (pkg in si$otherPkgs) lines <- c(lines,
                                       paste("- ", pkg$Package, " (", pkg$Version, ")", sep = ""))
  return(paste(lines, collapse = "\n"))
}

#'
#' @export
verifyDependencies <- function() {
  expected <- RJSONIO::fromJSON("renv.lock")
  expected <- dplyr::bind_rows(expected[[2]])
  basePackages <- rownames(installed.packages(priority = "base"))
  expected <- expected[!expected$Package %in% basePackages, ]
  observedVersions <- sapply(sapply(expected$Package, packageVersion), paste, collapse = ".")
  expectedVersions <- sapply(sapply(expected$Version, numeric_version), paste, collapse = ".")
  mismatchIdx <- which(observedVersions != expectedVersions)
  if (length(mismatchIdx) > 0) {

    lines <- sapply(mismatchIdx, function(idx) sprintf("- Package %s version %s should be %s",
                                                       expected$Package[idx],
                                                       observedVersions[idx],
                                                       expectedVersions[idx]))
    message <- paste(c("Mismatch between required and installed package versions. Did you forget to run renv::restore()?",
                       lines),
                     collapse = "\n")
    stop(message)
  }
  return("Dependencies Verified")
}
