#' @export
getThisPackageName <- function() {
  return("Covid19SubjectsAesiIncidenceRate")
}

#' @export
readCsv <- function(resourceFile, packageName = getThisPackageName(), col_types = readr::cols()) {
  packageName <- getThisPackageName()
  pathToCsv <- system.file(resourceFile, package = packageName, mustWork = TRUE)
  fileContents <- readr::read_csv(pathToCsv, col_types = col_types, lazy = FALSE)
  return(fileContents)
}
