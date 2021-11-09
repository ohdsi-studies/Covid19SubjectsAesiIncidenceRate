# Adverse Events of Special Interest within COVID-19 Subjects
# ##############################################################################

################################
# SETUP
################################
library(Covid19SubjectsAesiIncidenceRate)

options(andromedaTempFolder = "D:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = 'redshift',
                                                                server = paste0(Sys.getenv("DB_SERVER"),"/truven_ccae"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"),
                                                                port = 5439,
                                                                extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory")

outputFolder <- "D:/Git/GitHub/Covid19SubjectsAesiIncidenceRate/results"
cdmDatabaseSchema <- "cdm"
cohortDatabaseSchema <- "scratch"
cohortTable <- "aesi_cohort"
databaseId <- "DBID"
databaseName <- "Database Name"
databaseDescription <- "Database Description"

################################
# EXECUTE
################################
execute(connectionDetails = connectionDetails,
        outputFolder = outputFolder,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        databaseId = databaseId,
        createCohorts = FALSE,
        runCohortDiagnostics = TRUE)

#If CohortDiagnostics has been run, you can call the RShiney viewer like this:
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = file.path(outputFolder, "cohortDiagnostics"))

