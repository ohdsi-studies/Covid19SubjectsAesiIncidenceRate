getCohortDiagnostics <- function(cohortDiagnosticsFolder){

  dataFolder <- cohortDiagnosticsFolder

  # Local View
  CohortDiagnostics::preMergeDiagnosticsFiles(dataFolder, tempFolder = tempdir())
  #CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = dataFolder)

  # DB Load on OHDSI's server
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(Sys.getenv("shinydbServer"),Sys.getenv("shinydbDatabase"),sep = "/"),
    port = Sys.getenv("shinydbPort"),
    user = Sys.getenv("shinydbUser"),
    password = Sys.getenv("shinydbPW")
  )

  folderWithZipFilesToUpload <- dataFolder
  listOfZipFilesToUpload <-
    list.files(
      path = folderWithZipFilesToUpload,
      pattern = ".zip",
      full.names = TRUE,
      recursive = TRUE
    )

  for (i in (1:length(listOfZipFilesToUpload))) {
    CohortDiagnostics::uploadResults(
      connectionDetails = connectionDetails,
      schema = resultsSchema,
      zipFileName = listOfZipFilesToUpload[[i]]
    )
  }

  # If you need to truncate all the tables
  # SELECT CONCAT('TRUNCATE TABLE ',schemaname,'.',tablename,';')
  # FROM pg_catalog.pg_tables
  # WHERE schemaname != 'pg_catalog'
  # AND schemaname != 'information_schema'
  # AND schemaname = 'covid_aesi'
  # ORDER BY tablename ASC;
}
