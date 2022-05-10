#'
#' @export
#'


getResultsDataModelSpecifications <- function(fileName) {
  pathToCsv <-
    system.file("settings", fileName, package = "epi908")
  resultsDataModelSpecifications <-
    readr::read_csv(file = pathToCsv, col_types = readr::cols())
  return(resultsDataModelSpecifications)
}

checkFixColumnNames <-
  function(table,
           tableName,
           zipFileName,
           specifications = getResultsDataModelSpecifications("resultsDataModelSpecification.csv")) {
    if (tableName %in% c('cohort', 'phenotype_description',
                         'covariate_value', 'temporal_covariate_value')) {
      table <- fixTableMetadataForBackwardCompatibility(table = table,
                                                        tableName = tableName)
    }
    observeredNames <- colnames(table)[order(colnames(table))]

    tableSpecs <- specifications %>%
      dplyr::filter(.data$tableName == !!tableName)

    optionalNames <- tableSpecs %>%
      dplyr::filter(.data$optional == "Yes") %>%
      dplyr::select(.data$fieldName)

    expectedNames <- tableSpecs %>%
      dplyr::select(.data$fieldName) %>%
      dplyr::anti_join(dplyr::filter(optionalNames, !.data$fieldName %in% observeredNames),
                       by = "fieldName") %>%
      dplyr::arrange(.data$fieldName) %>%
      dplyr::pull()

    if (!isTRUE(all.equal(expectedNames, observeredNames))) {
      stop(
        sprintf(
          "Column names of table %s in zip file %s do not match specifications.\n- Observed columns: %s\n- Expected columns: %s",
          tableName,
          zipFileName,
          paste(observeredNames, collapse = ", "),
          paste(expectedNames, collapse = ", ")
        )
      )
    }
    return(table)
  }

checkAndFixDataTypes <-
  function(table,
           tableName,
           zipFileName,
           specifications = getResultsDataModelSpecifications("resultsDataModelSpecification.csv")) {
    tableSpecs <- specifications %>%
      filter(.data$tableName == !!tableName)

    observedTypes <- sapply(table, class)
    for (i in 1:length(observedTypes)) {
      fieldName <- names(observedTypes)[i]
      expectedType <-
        gsub("\\(.*\\)", "", tolower(tableSpecs$type[tableSpecs$fieldName == fieldName]))
      if (expectedType == "bigint" || expectedType == "float") {
        if (observedTypes[i] != "numeric" && observedTypes[i] != "double") {
          ParallelLogger::logDebug(
            sprintf(
              "Field %s in table %s in zip file %s is of type %s, but was expecting %s. Attempting to convert.",
              fieldName,
              tableName,
              zipFileName,
              observedTypes[i],
              expectedType
            )
          )
          table <- mutate_at(table, i, as.numeric)
        }
      } else if (expectedType == "int") {
        if (observedTypes[i] != "integer") {
          ParallelLogger::logDebug(
            sprintf(
              "Field %s in table %s in zip file %s is of type %s, but was expecting %s. Attempting to convert.",
              fieldName,
              tableName,
              zipFileName,
              observedTypes[i],
              expectedType
            )
          )
          table <- mutate_at(table, i, as.integer)
        }
      } else if (expectedType == "varchar") {
        if (observedTypes[i] != "character") {
          ParallelLogger::logDebug(
            sprintf(
              "Field %s in table %s in zip file %s is of type %s, but was expecting %s. Attempting to convert.",
              fieldName,
              tableName,
              zipFileName,
              observedTypes[i],
              expectedType
            )
          )
          table <- mutate_at(table, i, as.character)
        }
      } else if (expectedType == "date") {
        if (observedTypes[i] != "Date") {
          ParallelLogger::logDebug(
            sprintf(
              "Field %s in table %s in zip file %s is of type %s, but was expecting %s. Attempting to convert.",
              fieldName,
              tableName,
              zipFileName,
              observedTypes[i],
              expectedType
            )
          )
          table <- mutate_at(table, i, as.Date)
        }
      }
    }
    return(table)
  }

checkAndFixDuplicateRows <-
  function(table,
           tableName,
           zipFileName,
           specifications = getResultsDataModelSpecifications("resultsDataModelSpecification.csv")) {
    primaryKeys <- specifications %>%
      dplyr::filter(.data$tableName == !!tableName &
                      .data$primaryKey == "Yes") %>%
      dplyr::select(.data$fieldName) %>%
      dplyr::pull()
    duplicatedRows <- duplicated(table[, primaryKeys])
    if (any(duplicatedRows)) {
      warning(
        sprintf(
          "Table %s in zip file %s has duplicate rows. Removing %s records.",
          tableName,
          zipFileName,
          sum(duplicatedRows)
        )
      )
      return(table[!duplicatedRows,])
    } else {
      return(table)
    }
  }

appendNewRows <-
  function(data,
           newData,
           tableName,
           specifications = getResultsDataModelSpecifications("resultsDataModelSpecification.csv")) {
    if (nrow(data) > 0) {
      primaryKeys <- specifications %>%
        dplyr::filter(.data$tableName == !!tableName &
                        .data$primaryKey == "Yes") %>%
        dplyr::select(.data$fieldName) %>%
        dplyr::pull()
      newData <- newData %>%
        dplyr::anti_join(data, by = primaryKeys)
    }
    return(dplyr::bind_rows(data, newData))
  }


fixTableMetadataForBackwardCompatibility <- function(table, tableName) {
  if (tableName %in% c("cohort", "phenotype_description")) {
    if (!'metadata' %in% colnames(table)) {
      data <- list()
      for (i in (1:nrow(table))) {
        data[[i]] <- table[i,]
        colnamesDf <- colnames(data[[i]])
        metaDataList <- list()
        for (j in (1:length(colnamesDf))) {
          metaDataList[[colnamesDf[[j]]]] = data[[i]][colnamesDf[[j]]] %>% dplyr::pull()
        }
        data[[i]]$metadata <-
          RJSONIO::toJSON(metaDataList, pretty = TRUE, digits = 23)
      }
      table <- dplyr::bind_rows(data)
    }
    if ('referent_concept_id' %in% colnames(table)) {
      table <- table %>%
        dplyr::select(-.data$referent_concept_id)
    }
  }
  if (tableName %in% c('covariate_value', 'temporal_covariate_value')) {
    if (!'sum_value' %in% colnames(table)) {
      table$sum_value <- -1
    }
  }
  return(table)
}

preMergeDiagnosticsFiles <-
  function(dataFolder, tempFolder = tempdir()) {
    zipFiles <-
      dplyr::tibble(
        zipFile = list.files(
          dataFolder,
          pattern = ".zip",
          full.names = TRUE,
          recursive = TRUE
        ),
        unzipFolder = ""
      )
    ParallelLogger::logInfo("Merging ", nrow(zipFiles), " zip files.")

    unzipMainFolder <-
      tempfile("unzipTempFolder", tmpdir = tempFolder)
    dir.create(path = unzipMainFolder, recursive = TRUE)
    on.exit(unlink(unzipMainFolder, recursive = TRUE))

    for (i in 1:nrow(zipFiles)) {
      ParallelLogger::logInfo("- Unzipping ", basename(zipFiles$zipFile[i]))
      unzipFolder <-
        file.path(unzipMainFolder, sub(".zip", "", basename(zipFiles$zipFile[i])))
      dir.create(unzipFolder)
      zip::unzip(zipFiles$zipFile[i], exdir = unzipFolder)
      zipFiles$unzipFolder[i] <- unzipFolder
    }

    specifications <- getResultsDataModelSpecifications("resultsDataModelSpecification.csv")

    # Storing output in an environment for now. If things get too big, we may want to write
    # directly to CSV files for insertion into database:
    newEnvironment <- new.env()

    processTable <- function(tableName, env) {
      ParallelLogger::logInfo("Processing table ", tableName)
      csvFileName <- paste0(tableName, ".csv")
      data <- dplyr::tibble()
      for (i in 1:nrow(zipFiles)) {
        if (csvFileName %in% list.files(zipFiles$unzipFolder[i])) {
          newData <-
            readr::read_csv(
              file.path(zipFiles$unzipFolder[i], csvFileName),
              col_types = readr::cols(),
              guess_max = min(1e6)
            )
          if (nrow(newData) > 0) {
            newData <- checkFixColumnNames(
              table = newData,
              tableName = tableName,
              zipFileName = zipFiles$zipFile[i],
              specifications = specifications
            )
            newData <- checkAndFixDataTypes(
              table = newData,
              tableName = tableName,
              zipFileName = zipFiles$zipFile[i],
              specifications = specifications
            )
            newData <- checkAndFixDuplicateRows(
              table = newData,
              tableName = tableName,
              zipFileName = zipFiles$zipFile[i],
              specifications = specifications
            )
            data <- appendNewRows(
              data = data,
              newData = newData,
              tableName = tableName,
              specifications = specifications
            )

          }
        }
      }
      if (nrow(data) == 0) {
        ParallelLogger::logInfo("- No data found for table ", tableName)
      } else {
        colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
        assign(SqlRender::snakeCaseToCamelCase(tableName),
               data,
               envir = env)
      }
    }
    invisible(lapply(unique(specifications$tableName), processTable, env = newEnvironment))
    ParallelLogger::logInfo("Creating PreMerged.Rdata file. This might take some time.")
    save(
      list = ls(newEnvironment),
      envir = newEnvironment,
      compress = TRUE,
      compression_level = 2,
      file = file.path(dataFolder, "PreMerged.RData")
    )
    rm(list = ls(newEnvironment), envir = newEnvironment)
    ParallelLogger::logInfo("Merged data saved in ",
                            file.path(dataFolder, "PreMerged.RData"))
  }


preMergeIRFiles <- function(dataFolder, specificationFileName, tempFolder = tempdir()) {
  zipFiles <-
    dplyr::tibble(
      zipFile = list.files(
        dataFolder,
        pattern = ".zip",
        full.names = TRUE,
        recursive = TRUE
      ),
      unzipFolder = ""
    )
  ParallelLogger::logInfo("Merging ", nrow(zipFiles), " zip files.")

  unzipMainFolder <-
    tempfile("unzipTempFolder", tmpdir = tempFolder)
  dir.create(path = unzipMainFolder, recursive = TRUE)
  on.exit(unlink(unzipMainFolder, recursive = TRUE))

  for (i in 1:nrow(zipFiles)) {
    ParallelLogger::logInfo("- Unzipping ", basename(zipFiles$zipFile[i]))
    unzipFolder <-
      file.path(unzipMainFolder, sub(".zip", "", basename(zipFiles$zipFile[i])))
    dir.create(unzipFolder)
    zip::unzip(zipFiles$zipFile[i], exdir = unzipFolder)
    zipFiles$unzipFolder[i] <- unzipFolder
  }

  specifications <- getResultsDataModelSpecifications(specificationFileName)

  # Storing output in an environment for now. If things get too big, we may want to write
  # directly to CSV files for insertion into database:
  newEnvironment <- new.env()

  processTable <- function(tableName, env) {
    ParallelLogger::logInfo("Processing table ", tableName)
    csvFileName <- paste0(tableName, ".csv")
    data <- dplyr::tibble()
    for (i in 1:nrow(zipFiles)) {
      if (csvFileName %in% list.files(zipFiles$unzipFolder[i])) {
        newData <-
          readr::read_csv(
            file.path(zipFiles$unzipFolder[i], csvFileName),
            col_types = readr::cols(),
            guess_max = min(1e6)
          )
        if (nrow(newData) > 0) {
          newData <- checkFixColumnNames(
            table = newData,
            tableName = tableName,
            zipFileName = zipFiles$zipFile[i],
            specifications = specifications
          )
          newData <- checkAndFixDataTypes(
            table = newData,
            tableName = tableName,
            zipFileName = zipFiles$zipFile[i],
            specifications = specifications
          )
          # newData <- checkAndFixDuplicateRows(
          #   table = newData,
          #   tableName = tableName,
          #   zipFileName = zipFiles$zipFile[i],
          #   specifications = specifications
          # )
          data <- appendNewRows(
            data = data,
            newData = newData,
            tableName = tableName,
            specifications = specifications
          )

        }
      }
    }
    if (nrow(data) == 0) {
      ParallelLogger::logInfo("- No data found for table ", tableName)
    } else {
      colnames(data) <- SqlRender::snakeCaseToCamelCase(colnames(data))
      assign(SqlRender::snakeCaseToCamelCase(tableName),
             data,
             envir = env)
    }
  }
  processTable("database",newEnvironment)
  invisible(lapply(unique(specifications$tableName), processTable, env = newEnvironment))
  ParallelLogger::logInfo("Creating PreMerged.Rdata file. This might take some time.")
  save(
    list = ls(newEnvironment),
    envir = newEnvironment,
    compress = TRUE,
    compression_level = 2,
    file = file.path(dataFolder, "PreMerged.RData")
  )
  rm(list = ls(newEnvironment), envir = newEnvironment)
  ParallelLogger::logInfo("Merged data saved in ",
                          file.path(dataFolder, "PreMerged.RData"))
}



uploadResults <- function(connectionDetails = NULL,
                          schema,
                          zipFileName,
                          forceOverWriteOfSpecifications = FALSE,
                          purgeSiteDataBeforeUploading = TRUE,
                          tempFolder = tempdir()) {
  start <- Sys.time()
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  unzipFolder <- tempfile("unzipTempFolder", tmpdir = tempFolder)
  dir.create(path = unzipFolder, recursive = TRUE)
  on.exit(unlink(unzipFolder, recursive = TRUE), add = TRUE)

  ParallelLogger::logInfo("Unzipping ", zipFileName)
  zip::unzip(zipFileName, exdir = unzipFolder)

  specifications <- getResultsDataModelSpecifications("resultsDataModelSpecification.csv")

  if (purgeSiteDataBeforeUploading) {
    database <-
      readr::read_csv(file = file.path(unzipFolder, "database.csv"),
                      col_types = readr::cols())
    colnames(database) <-
      SqlRender::snakeCaseToCamelCase(colnames(database))
    databaseId <- database$databaseId
  }

  uploadTable <- function(tableName) {
    ParallelLogger::logInfo("Uploading table ", tableName)

    primaryKey <- specifications %>%
      filter(.data$tableName == !!tableName &
               .data$primaryKey == "Yes") %>%
      select(.data$fieldName) %>%
      pull()

    if (purgeSiteDataBeforeUploading &&
        "database_id" %in% primaryKey) {
      deleteAllRecordsForDatabaseId(
        connection = connection,
        schema = schema,
        tableName = tableName,
        databaseId = databaseId
      )
    }

    csvFileName <- paste0(tableName, ".csv")
    if (csvFileName %in% list.files(unzipFolder)) {
      env <- new.env()
      env$schema <- schema
      env$tableName <- tableName
      env$primaryKey <- primaryKey
      if (purgeSiteDataBeforeUploading &&
          "database_id" %in% primaryKey) {
        env$primaryKeyValuesInDb <- NULL
      } else {
        sql <- "SELECT DISTINCT @primary_key FROM @schema.@table_name;"
        sql <- SqlRender::render(
          sql = sql,
          primary_key = primaryKey,
          schema = schema,
          table_name = tableName
        )
        primaryKeyValuesInDb <-
          DatabaseConnector::querySql(connection, sql)
        colnames(primaryKeyValuesInDb) <-
          tolower(colnames(primaryKeyValuesInDb))
        env$primaryKeyValuesInDb <- primaryKeyValuesInDb
      }

      uploadChunk <- function(chunk, pos) {
        ParallelLogger::logInfo("- Preparing to upload rows ",
                                pos,
                                " through ",
                                pos + nrow(chunk) - 1)

        chunk <- checkFixColumnNames(
          table = chunk,
          tableName = env$tableName,
          zipFileName = zipFileName,
          specifications = specifications
        )
        chunk <- checkAndFixDataTypes(
          table = chunk,
          tableName = env$tableName,
          zipFileName = zipFileName,
          specifications = specifications
        )
        chunk <- checkAndFixDuplicateRows(
          table = chunk,
          tableName = env$tableName,
          zipFileName = zipFileName,
          specifications = specifications
        )

        # Primary key fields cannot be NULL, so for some tables convert NAs to empty or zero:
        toEmpty <- specifications %>%
          filter(
            .data$tableName == env$tableName &
              .data$emptyIsNa == "No" & grepl("varchar", .data$type)
          ) %>%
          select(.data$fieldName) %>%
          pull()
        if (length(toEmpty) > 0) {
          chunk <- chunk %>%
            dplyr::mutate_at(toEmpty, naToEmpty)
        }

        tozero <- specifications %>%
          filter(
            .data$tableName == env$tableName &
              .data$emptyIsNa == "No" &
              .data$type %in% c("int", "bigint", "float")
          ) %>%
          select(.data$fieldName) %>%
          pull()
        if (length(tozero) > 0) {
          chunk <- chunk %>%
            dplyr::mutate_at(tozero, naToZero)
        }

        # Check if inserting data would violate primary key constraints:
        if (!is.null(env$primaryKeyValuesInDb)) {
          primaryKeyValuesInChunk <- unique(chunk[env$primaryKey])
          duplicates <- inner_join(env$primaryKeyValuesInDb,
                                   primaryKeyValuesInChunk,
                                   by = env$primaryKey)
          if (nrow(duplicates) != 0) {
            if ("database_id" %in% env$primaryKey ||
                forceOverWriteOfSpecifications) {
              ParallelLogger::logInfo(
                "- Found ",
                nrow(duplicates),
                " rows in database with the same primary key ",
                "as the data to insert. Deleting from database before inserting."
              )
              deleteFromServer(
                connection = connection,
                schema = env$schema,
                tableName = env$tableName,
                keyValues = duplicates
              )

            } else {
              ParallelLogger::logInfo(
                "- Found ",
                nrow(duplicates),
                " rows in database with the same primary key ",
                "as the data to insert. Removing from data to insert."
              )
              chunk <- chunk %>%
                anti_join(duplicates, by = env$primaryKey)
            }
            # Remove duplicates we already dealt with:
            env$primaryKeyValuesInDb <- env$primaryKeyValuesInDb %>%
              anti_join(duplicates, by = env$primaryKey)
          }
        }
        if (nrow(chunk) == 0) {
          ParallelLogger::logInfo("- No data left to insert")
        } else {
          DatabaseConnector::insertTable(
            connection = connection,
            tableName = paste(env$schema, env$tableName, sep = "."),
            data = chunk,
            dropTableIfExists = FALSE,
            createTable = FALSE,
            tempTable = FALSE,
            progressBar = TRUE
          )
        }
      }
      readr::read_csv_chunked(
        file = file.path(unzipFolder, csvFileName),
        callback = uploadChunk,
        chunk_size = 1e7,
        col_types = readr::cols(),
        guess_max = 1e6,
        progress = FALSE
      )

      # chunk <- readr::read_csv(file = file.path(unzipFolder, csvFileName),
      # col_types = readr::cols(),
      # guess_max = 1e6)

    }
  }
  invisible(lapply(unique(specifications$tableName), uploadTable))
  delta <- Sys.time() - start
  writeLines(paste("Uploading data took", signif(delta, 3), attr(delta, "units")))
}

deleteAllRecordsForDatabaseId <- function(connection,
                                          schema,
                                          tableName,
                                          databaseId) {
  sql <-
    "SELECT COUNT(*) FROM @schema.@table_name WHERE database_id = '@database_id';"
  sql <- SqlRender::render(
    sql = sql,
    schema = schema,
    table_name = tableName,
    database_id = databaseId
  )
  databaseIdCount <-
    DatabaseConnector::querySql(connection, sql)[, 1]
  if (databaseIdCount != 0) {
    ParallelLogger::logInfo(
      sprintf(
        "- Found %s rows in  database with database ID '%s'. Deleting all before inserting.",
        databaseIdCount,
        databaseId
      )
    )
    sql <-
      "DELETE FROM @schema.@table_name WHERE database_id = '@database_id';"
    sql <- SqlRender::render(
      sql = sql,
      schema = schema,
      table_name = tableName,
      database_id = databaseId
    )
    DatabaseConnector::executeSql(connection,
                                  sql,
                                  progressBar = FALSE,
                                  reportOverallTime = FALSE)
  }
}

naToEmpty <- function(x) {
  x[is.na(x)] <- ""
  return(x)
}

naToZero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}


enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor)/nrow(data), 1)
    ParallelLogger::logInfo("   censoring ",
                            sum(toCensor),
                            " values (",
                            percent,
                            "%) from ",
                            fieldName,
                            " because value below minimum")
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

