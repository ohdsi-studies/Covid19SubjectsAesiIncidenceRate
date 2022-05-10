# ##############################################################################
# Adverse Events of Special Interest within COVID-19 Subjects
# ##############################################################################

install.packages("renv")
#download.file("https://raw.githubusercontent.com/ohdsi-studies/Covid19SubjectsAesiIncidenceRate/anaphylaxis/renv.lock", "renv.lock")
download.file("https://raw.githubusercontent.com/ohdsi-studies/Covid19SubjectsAesiIncidenceRate/master/renv.lock", "renv.lock")
renv::init()
renv::restore()
library(Covid19SubjectsAesiIncidenceRate)


# --- SETUP --------------------------------------------------------------------


databases <- read.csv("extras/XX_databases.csv",header=TRUE)
database <- databases[3,]

options(andromedaTempFolder = "D:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

# Details for connecting to the server:
connectionDetails <-
  DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                             server = database$server, #paste0(Sys.getenv("DB_SERVER3"),"/optum_extended_ses"),
                                             user = keyring::key_get("redShiftUserName"),
                                             password = keyring::key_get("redShiftPassword"),
                                             port = 5439,
                                             extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)

outputFolder <- database$outputFolder
cdmDatabaseSchema <- database$cdmDatabaseSchema
cohortDatabaseSchema <- database$cohortDatabaseSchema
cohortTablePrefix <- database$cohortTablePrefix
cohortTable <- database$cohortTable
databaseId <- database$databaseId
databaseName <- database$databaseName
databaseDescription <- database$databaseDescription

# --- EXECUTE ------------------------------------------------------------------
Covid19SubjectsAesiIncidenceRate::execute(connectionDetails = connectionDetails,
                                          outputFolder = outputFolder,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTablePrefix = cohortTablePrefix,
                                          cohortTable = cohortTable,
                                          databaseId = databaseId,
                                          databaseName = databaseName,
                                          databaseDescription = databaseDescription,
                                          createCohortsAndRef = TRUE,
                                          runCohortDiagnostics = TRUE,
                                          runIR = TRUE)

# --- SHARE RESULTS ------------------------------------------------------------
# Upload the results to the OHDSI SFTP server:
privateKeyFileName <- "<file>"
userName <- "<name>"
Covid19SubjectsAesiIncidenceRate::uploadDiagnosticsResults(file.path(outputFolder,"cohortDiagnostics"), privateKeyFileName, userName)
Covid19SubjectsAesiIncidenceRate::uploadStudyResults(file.path(outputFolder, "incidenceRate"), privateKeyFileName, userName)

# --- VIEW COHORT DIAGNOSTICS --------------------------------------------------
# If CohortDiagnostics has been run, you can call the RShiney viewer like this:
CohortDiagnostics::preMergeDiagnosticsFiles(file.path("D:/Git/BitBucket/epi_974/Covid19SubjectsAesiIncidenceRate/results", "cohortDiagnostics"))
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = file.path(outputFolder,"cohortDiagnostics"))
