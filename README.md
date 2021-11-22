Adverse Events of Special Interest within COVID-19 Subjects
=============

<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **COVID-19**
- Study lead: **Erica A Voss**
- Study lead forums tag: **[ericaVoss](https://forums.ohdsi.org/u/ericaVoss)**
- Study start date: **November 2, 2021**
- Study end date: **-**
- Protocol: [AESIs in COVID-19 Subjects Protocol](https://ohdsi-studies.github.io/Covid19SubjectsAesiIncidenceRate/Protocol.html)
- Publications: **-**
- Results explorer: **-**

Extending on our previous work by Li et al. [1](https://github.com/ohdsi-studies/Covid19VaccineAesiIncidenceRate) [2](https://pubmed.ncbi.nlm.nih.gov/33791732/) in understanding the incidence rates of adverse events of special interest (AESI) for COVID-19, this work will look at the rates of these AESIs in patients who had COVDI-19 disease.

# Requirements
- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

# How to Run
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java. 

2. Create an empty folder or new RStudio project, and in R, use the following code to install the study package and its dependencies:

    ```r
    install.packages("renv")
    download.file("https://raw.githubusercontent.com/ohdsi-studies/Covid19SubjectsAesiIncidenceRate/master/renv.lock", "renv.lock")
    renv::init()
    ```

3. If/When asked if the project already has a lockfile select "1: Restore the project from the lockfile.".

4. You can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:
	
	```r
	# --- SETUP --------------------------------------------------------------------
	library(Covid19SubjectsAesiIncidenceRate)

	options(andromedaTempFolder = "D:/andromedaTemp")
	options(sqlRenderTempEmulationSchema = NULL)

	# Details for connecting to the server:
	# See ?DatabaseConnector::createConnectionDetails for help
	connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
																	server = "some.server.com/ohdsi",
																	user = "joe",
																	password = "secret")

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

	```
    
    This step will generate the following tables in the cohortDatabaseSchema:
     - <cohortTable>
     - <cohortTablePrefix>_ir_summary
     - <cohortTablePrefix>_outcome
     - <cohortTablePrefix>_outcome_ref
     - <cohortTablePrefix>_subgroup
     - <cohortTablePrefix>_subgroup_ref
     - <cohortTablePrefix>_target
     - <cohortTablePrefix>_target_ref
     - <cohortTablePrefix>_time_at_risk

5. (OPTIONAL) If you want to view your CohortDiagnostics results, run the following:

    ```r
    # --- VIEW COHORT DIAGNOSTICS --------------------------------------------------
    # If CohortDiagnostics has been run, you can call the RShiney viewer like this:
    CohortDiagnostics::launchDiagnosticsExplorer(dataFolder = file.path(outputFolder,"cohortDiagnostics"))
    ```	

6. Upload the files ```results/cohortDiagnostics/Results_<DatabaseId>.zip``` and ```results/incidenceRate/Results_IR_<DatabaseId>.zip``` in the output folder to the study coordinator:
 
    ```r
    privateKeyFileName <- "<file>"
    userName <- "<name>"
    Covid19SubjectsAesiIncidenceRate::uploadStudyResults(file.path(outputFolder,"cohortDiagnostics"), privateKeyFileName, userName)
    Covid19SubjectsAesiIncidenceRate::uploadStudyResults(file.path(outputFolder, "incidenceRate"), privateKeyFileName, userName)
    ```
	
    Where ```<file>``` and ```<name>``` are the credentials provided to you personally by the study coordinator.

# License 
The Covid19SubjectsAesiIncidenceRate package is licensed under Apache License 2.0.

# Development

Covid19SubjectsAesiIncidenceRate was developed in ATLAS and R Studio.
