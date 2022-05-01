#'
#' @export
#'
#'
whatsubgroupsToCensor <- function(){
  #Censorship by Other
  df1 <- c('IBM_CCAE','IBM_MDCD')
  df2 <- c('71',	'81',	'91',	'72',	'82',	'92')
  df3 <- expand.grid(df1,df2)
  colnames(df3) <- c('DB','subgroupCohortDefinitionId')

  df10 <- c('IBM_MDCR')
  df20 <- c('21','31','41','51','61','101','111','121','22','32','42','52','62','102','112','122')
  df30 <- expand.grid(df10,df20)
  colnames(df30) <- c('DB','subgroupCohortDefinitionId')

  censorSubgroupCohortDefinitionId <- rbind(df3, df30)

  return(censorSubgroupCohortDefinitionId)
}
