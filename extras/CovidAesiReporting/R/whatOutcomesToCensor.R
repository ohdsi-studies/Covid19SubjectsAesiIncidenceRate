#'
#' @export
#'
#'
whatOutcomesToCensor <- function(dataFolder, censorshipFile,censoredResults=0){
  #Censorship by Phenotype
  censorshipDf <- readxl::read_excel(path = censorshipFile, sheet= "CENSORSHIP-PHENOTYPES")
  colnames(censorshipDf) <- c('DB','562','563', '565', '566', 'DIV', '411', '405',	'406',	'402',	'386',	'385',	'381', '349',	'568',	'347', '346',	'345',	'343',	'340',	'339',	'335',	'547', '620', '6246')
  censorshipDf <- censorshipDf[4:nrow(censorshipDf),]
  censorshipDf <- subset(censorshipDf, select = -c(DIV))
  dbNames <- censorshipDf[,1]
  censorshipDf[censorshipDf == 'No Censorship'] <- "NOT_CENSORED"
  censorshipDf[censorshipDf != 'NOT_CENSORED'] <- 'CENSORED'
  censorshipDf[is.na(censorshipDf)] <- 'CENSORED'
  censorshipDf$DB <- dbNames$DB

  censorshipDfPivot <- censorshipDf %>% pivot_longer(!DB,names_to = "PHENOTYPE",values_to = "CENSORED")
  censoredDF <- censorshipDfPivot[censorshipDfPivot$CENSORED == 'NOT_CENSORED',]

  if(censoredResults){
    censoredDF <- censorshipDfPivot[censorshipDfPivot$CENSORED == 'CENSORED',]
  }

  return(censoredDF)
}
