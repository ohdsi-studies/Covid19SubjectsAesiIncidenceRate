# ##############################################################################
# Adverse Events of Special Interest within COVID-19 Subjects
# ##############################################################################

# --- SETUP --------------------------------------------------------------------
library(Covid19SubjectsAesiIncidenceRate)

options(andromedaTempFolder = "D:/andromedaTemp")
options(sqlRenderTempEmulationSchema = NULL)

# Details for connecting to the server:
# See ?DatabaseConnector::createConnectionDetails for help
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server = "some.server.com/ohdsi",
                                                                user = "joe",
                                                                password = "secret",
                                                                pathToDriver = "D:/drivers")

outputFolder <- "D:/Covid19SubjectsAesiIncidenceRate/results"
cdmDatabaseSchema <- "cdm_synpuf"
cohortDatabaseSchema <- "scratch.dbo"
cohortTablePrefix <- "aesi"
cohortTable <- "aesi_cohort"
databaseId <- "synpuf"
databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."

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
CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = file.path(outputFolder,"cohortDiagnostics"))
