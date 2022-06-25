#'
#' @export
#'
#'
metaAnalysis <- function(resultsFolder,incidenceAnalysisFile,outcomeSortOrder){
  library(tidyverse)
  library(metafor)
  library(meta)
  library(reshape2)

  # variables
  resultsFolder <- resultsFolder
  incidenceAnalysis <- read.csv(incidenceAnalysisFile)


  ## subgroup ref
  subgroup_ref <- incidenceAnalysis %>% dplyr::select(subgroupCohortDefinitionId, subgroupName) %>%
    distinct() %>%
    mutate(sex_group=case_when(grepl('Male',subgroupName) ~ 'Male',
                               grepl('Female',subgroupName) ~'Female',
                               TRUE ~ 'NA'),
           age_lb=as.numeric(sub("\\D*(\\d+).*", "\\1",subgroupName)),
           age_ub=as.numeric(sub(".*\\b(\\d+).*", "\\1", subgroupName)),
           age_group= paste(age_lb, "-", age_ub)
    ) %>% arrange(age_lb) %>%
    group_by(subgroupCohortDefinitionId) %>% slice(1) %>% ungroup()

  subgroup_ref$age_group = ifelse(subgroup_ref$subgroupCohortDefinitionId %in% c(1,2),'NA',subgroup_ref$age_group  )
  subgroup_ref$age_group = ifelse(subgroup_ref$subgroupCohortDefinitionId == 0,'All',subgroup_ref$age_group  )
  subgroup_ref$age_group= factor(subgroup_ref$age_group, levels=c(   "0 - 5"   , "6 - 17" ,  "18 - 34" , "35 - 54",  "55 - 64" , "65 - 74", "75 - 84" , "85 - 114","All","NA"))
  subgroup_ref <- subset(subgroup_ref, subgroupCohortDefinitionId < 100)


  ## outcome event ref
  outcome_ref <- incidenceAnalysis %>% dplyr::select(outcomeId, outcomeName,cleanWindow) %>%
    distinct_all() %>% arrange(outcomeName) %>%
    mutate(outcome_main_name = case_when( grepl( 'AutoimmuneHep',tolower(outcomeName)) ~ 'Autoimmune Hepatitis')
    )


  ## study design ref
  method_ref <- incidenceAnalysis %>%
    dplyr::select(targetCohortDefinitionId,targetName,timeAtRiskId,timeAtRiskStartOffset,timeAtRiskStartIndex,timeAtRiskEndOffset,timeAtRiskEndIndex) %>%
    distinct_all()


  ## study_ref
  study_ref_all <- incidenceAnalysis  %>%
    select(targetCohortDefinitionId,timeAtRiskId,outcomeCohortDefinitionId,subgroupCohortDefinitionId)%>%
    distinct()


  ## database ref
  database_ref <-  incidenceAnalysis %>%  dplyr::select(databaseName) %>% distinct() %>%
    mutate(databaseNameU = toupper(databaseName),
           db_name = (databaseNameU))

  ## add ref tables back to data
  IR_use <- incidenceAnalysis %>%
    mutate(cell_ls5=if_else(numOutcomes < 5 | numPersonsWOutcome < 5, 1 ,0),   #### flag small cell size #####
           IR_P_100000py=case_when(cell_ls5==0 ~incidenceRateP100py*1000), #### format to per 100,000 person-years ###
           uncensoredOutcomes = round(personYears*incidenceRateP100py/100)
    ) %>%
    left_join(select(outcome_ref, - cleanWindow, -outcomeName), by="outcomeId") %>%
    left_join(select(subgroup_ref,- age_lb, - age_ub, -subgroupName),by='subgroupCohortDefinitionId') %>%
    left_join(select(database_ref,-databaseNameU), by='databaseName')

  IR_use_for_MA <- IR_use %>%
    filter(targetCohortDefinitionId %in% c(563),  #COVID DX & PX
           timeAtRiskId == 6,  #90d
           subgroupCohortDefinitionId > 20, #age/sex strata
           outcomeCohortDefinitionId %in% c(411,405,406,402,386,385,381,349,568,347,346,345,343,340,339,335,547),
           numPersonsAtRisk > 0, # remove records for strata with no persons at risk
           personYears > 0
    )


  rand_te <- function(data){
    random.meta <- metarate(data=data, event=uncensoredOutcomes, time=personYears, studlab = db_name,
                            sm = "IRLN",
                            comb.random = TRUE
                            , method.tau = "DL")

    random.te <- random.meta[["TE.random"]]
    random.te.lower <-  random.meta[["lower.random"]]
    random.te.upper <-  random.meta[["upper.random"]]
    seTE.random <- random.meta[["seTE.random"]]
    tau2 <- random.meta[["tau2"]]
    se.tau2 <- random.meta[["se.tau2"]]
    tau <- random.meta[["tau"]]
    lower.predict<- random.meta[["lower.predict"]]
    upper.predict<- random.meta[["upper.predict"]]
    seTE.predict<- random.meta[["seTE.predict"]]
    meta.out <- data.frame(random.te,random.te.lower,random.te.upper,seTE.random,
                           tau2,se.tau2,tau,
                           lower.predict, upper.predict, seTE.predict)
    return(meta.out)
  }

  #run meta-analysis for each T/S/O combination
  meta_result0 <- IR_use_for_MA %>%
    group_by(targetCohortDefinitionId,timeAtRiskId,outcomeCohortDefinitionId,subgroupCohortDefinitionId) %>%
    do(data.frame(rand_te(.)))

  meta_result <- meta_result0 %>% mutate(
    ir.rand=exp(random.te)*100000,
    ir.rand.l=exp(random.te.lower)*100000,
    ir.rand.u=exp(random.te.upper)*100000,
    ir.predict.lower=exp(lower.predict)*100000,
    ir.predict.upper=exp(upper.predict)*100000
  )

  #meta_result <- meta_result %>% filter (subGroupName != NA)

  #add refs names
  meta_result <-  meta_result %>%
    left_join(select(method_ref,targetCohortDefinitionId, targetName,timeAtRiskId),
              by=c('targetCohortDefinitionId','timeAtRiskId'))%>%
    left_join(rename(outcome_ref, outcomeCohortDefinitionId =outcomeId), by="outcomeCohortDefinitionId") %>%
    left_join(select(subgroup_ref,- age_lb, - age_ub),by='subgroupCohortDefinitionId')

  # format meta-analysis results in table with columns for age*sex
  table_by_age_sex <-  meta_result %>% ungroup() %>%
    mutate(pe = ir.rand,
           lb = ir.predict.lower,
           ub = ir.predict.upper
    )

  table_by_age_sex <- table_by_age_sex %>% select(outcomeName,
                                                  sex_group,age_group,pe, lb, ub ) %>%
    # dcast(outcome_main_name+outcomeName ~ age_by_sex , value.var = c("ir.rand"))
    pivot_wider(names_from = c(sex_group,age_group), names_glue = "{age_group}_{sex_group}_{.value}", values_from = c(pe,lb,ub))

  table_by_age_sex <- table_by_age_sex %>%
    relocate(outcomeName, '0 - 5_Male_pe', '0 - 5_Male_lb', '0 - 5_Male_ub', '0 - 5_Female_pe', '0 - 5_Female_lb', '0 - 5_Female_ub',
             '6 - 17_Male_pe', '6 - 17_Male_lb', '6 - 17_Male_ub', '6 - 17_Female_pe', '6 - 17_Female_lb', '6 - 17_Female_ub',
             '18 - 34_Male_pe', '18 - 34_Male_lb', '18 - 34_Male_ub', '18 - 34_Female_pe', '18 - 34_Female_lb', '18 - 34_Female_ub',
             '35 - 54_Male_pe', '35 - 54_Male_lb', '35 - 54_Male_ub', '35 - 54_Female_pe', '35 - 54_Female_lb', '35 - 54_Female_ub',
             '55 - 64_Male_pe', '55 - 64_Male_lb', '55 - 64_Male_ub', '55 - 64_Female_pe', '55 - 64_Female_lb', '55 - 64_Female_ub',
             '65 - 74_Male_pe', '65 - 74_Male_lb', '65 - 74_Male_ub', '65 - 74_Female_pe', '65 - 74_Female_lb', '65 - 74_Female_ub',
             '75 - 84_Male_pe', '75 - 84_Male_lb', '75 - 84_Male_ub', '75 - 84_Female_pe', '75 - 84_Female_lb', '75 - 84_Female_ub',
             '85 - 114_Male_pe', '85 - 114_Male_lb', '85 - 114_Male_ub', '85 - 114_Female_pe', '85 - 114_Female_lb', '85 - 114_Female_ub'
    )

  table_by_age_sex_for_csv <- apply(table_by_age_sex,2,as.character)
  write.csv(table_by_age_sex_for_csv,file.path(resultsFolder, "table_by_age_sex.csv"), row.names = T)
  #write.csv(table_by_age_sex,file.path(resultsFolder, "table_by_age_sex.csv"), row.names = T)

  # provide table with database-specific estimates
  table_by_source <- IR_use_for_MA %>%
    dplyr::select(db_name, outcomeId, outcomeName, age_group, sex_group, numOutcomes, IR_P_100000py) %>%
    arrange(db_name, outcomeName, sex_group, age_group)

  table_by_source <- table_by_source %>%
    dcast( outcomeName+age_group ~ db_name+sex_group, value.var = "IR_P_100000py", fun.aggregate = sum)

  write.csv(table_by_source,file.path(resultsFolder, "table_by_source.csv"), row.names = T)

  # Make table for paper--------------------------------------------------------
  table_by_age_sex_for_csv_df <- as.data.frame(table_by_age_sex_for_csv)

  #build table-happy concatenation
  df <- as.data.frame(table_by_age_sex_for_csv_df$outcomeName)
  colnames(df) <- c("outcomeName")

  for(i in 1:(((ncol(table_by_age_sex_for_csv_df)-1)/3)-1)){
    ir <-2 + (i-1)*3
    lb <-3 + (i-1)*3
    ub <-4 + (i-1)*3

    dfCol <- as.data.frame(paste0(
      round(as.numeric(table_by_age_sex_for_csv[,ir]),0)," (",
      round(as.numeric(table_by_age_sex_for_csv[,lb]),0)," to ",
      round(as.numeric(table_by_age_sex_for_csv[,ub]),0),")"))
    colnames(dfCol) <- colnames(table_by_age_sex_for_csv)[ir]
    colName1 <- colnames(table_by_age_sex_for_csv)[ir]
    colName2 <- paste0(colnames(table_by_age_sex_for_csv)[ir],"_COLOR")

    #CIOMS
    cioms <- as.data.frame(as.numeric(table_by_age_sex_for_csv[,ir]))
    colnames(cioms) <- c("ir")

    cioms<- mutate(cioms, color = ifelse(ir < 10 , "DARK GREEN",
                              ifelse(ir >= 10 & ir < 100, "LIGHT GREEN",
                                     ifelse(ir >= 100 & ir < 1000,"YELLOW",
                                            ifelse(ir >= 1000 & ir < 10000,"ORANGE",
                                                   ifelse(ir >= 10000,"RED","ERROR"))))))

    dfCol <- cbind(dfCol,cioms$color)
    colnames(dfCol) <- c(colName1,colName2)

    df <- cbind(df,dfCol)
  }

  #stack male and female
  df_prep <- as.data.frame(table_by_age_sex_for_csv_df$outcomeName)
  colnames(df_prep) <- "outcomeName"
  df_prep$gender <- "Female"

  df_female <- as.data.frame(dplyr::select(df,matches("female")))
  df_female <- cbind(df_prep, df_female)
  colnames(df_female) <- sub("_Female","",colnames(df_female))

  df_prep$gender <- "Male"
  df_male <- as.data.frame(dplyr::select(df,matches("_male")))
  df_male <- cbind(df_prep, df_male)
  colnames(df_male) <- sub("_Male","",colnames(df_male))

  df_female_male <- rbind(df_female,df_male)
  df_female_male <- merge(df_female_male, outcomeSortOrder, by = 'outcomeName', all.x = "TRUE")
  df_female_male <- df_female_male[order(df_female_male$outcomeNameSortOrder,df_female_male$gender),]

  write.csv(df_female_male,file.path(resultsFolder, "table_by_age_sex_pretty.csv"), row.names = T)
}
