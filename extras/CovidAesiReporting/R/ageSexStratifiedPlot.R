ageSexStratifiedPlot <- function(resultsFolder, irFolder){

  library("ggplot2")
  library("viridis")
  library("scales")
  library ("dplyr")

  ##################
  #### FIGURE 1 ####
  ##################
  fileName <- paste0(resultsFolder,"/AGE_SEX_STRATIFIED_IR.pdf")

  # Import data
  Data1 <- read.csv(paste0(irFolder,"/incidenceAnalysisCensoredCovidVsGeneralForPlot.csv"))
  names (Data1)
  table (Data1$targetName)
  table (Data1$timeAtRiskId)
  table (Data1$ageGroup)

  # Rename age groups + set levels
  Data1[grep ("000-005", Data1$ageGroup), "AgeGroup_New"] <- "0-5"
  Data1[grep ("006-017", Data1$ageGroup), "AgeGroup_New"] <- "6-17"
  Data1[grep ("018-034", Data1$ageGroup), "AgeGroup_New"] <- "18-34"
  Data1[grep ("035-054", Data1$ageGroup), "AgeGroup_New"] <- "35-54"
  Data1[grep ("055-064", Data1$ageGroup), "AgeGroup_New"] <- "55-64"
  Data1[grep ("065-074", Data1$ageGroup), "AgeGroup_New"] <- "65-74"
  Data1[grep ("075-084", Data1$ageGroup), "AgeGroup_New"] <- "75-84"
  Data1[grep ("085-114", Data1$ageGroup), "AgeGroup_New"] <- ">=85"
  table (Data1$AgeGroup_New, useNA = "ifany")
  Data1$AgeGroup_New <- factor(Data1$AgeGroup_New, levels = c("0-5", "6-17", "18-34", "35-54", "55-64", "65-74", "75-84", ">=85"))

  # Order outcomes by incidence (highest to lowest)
  table (Data1$outcomeName)
  Data1$outcomeName <- factor(Data1$outcomeName,levels = c("Acute Myocardial Infarction","Non-hemorrhagic Stroke","Deep Vein Thrombosis (DVT)","Pulmonary Embolism",
                                                           "Hemorrhagic Stroke","Bells Palsy","Appendicitis","Myocarditis Pericarditis",
                                                           "Thrombosis with Thrombocytopenia (TWT)","Immune Thrombocytopenia (ITP)","Anaphylaxis","Narcolepsy",
                                                           "Disseminated Intravascular Coagulation","Encephalomyelitis","Guillain Barre Syndrome","Transverse Myelitis"
  ))

  # Order databases by types
  # Databases 1-8: Administrative claims
  # Databases 9-22: EHR
  # Databases 23-27: GP/primary care
  table (Data1$databaseName)
  Data1$databaseName <- factor(Data1$databaseName, levels = c("IBM_CCAE","IBM_MDCD","IBM_MDCR","IQVIA_OPENCLAIMS","IQVIA_PHARMETRICS","JMDC","OPTUM_SES","U_OF_TARTU",
                                                              "APHM","CU_AMC","CUIMC","FIIBAP","HIC","IMASIS","IU","MHD","OPTUM_EHR","STARR","UCCS","UCHDW","UK_BIOBANK",
                                                              "CPRD_AURUM","IPCI","IQVIA_FRANCE_DA","IQVIA_GERMANY_DA","SIDIAP"))

  # Filter out data points </=0
  Data2 <- subset(Data1, c(Data1$incidenceRateP100pyGeneral>0.00 & Data1$incidenceRateP100py>0.00))

  # Change to 100K PY
  Data2$IR_COVID <- Data2$incidenceRateP100py*1000
  Data2$IR_POP <- Data2$incidenceRateP100pyGeneral*1000

  # Plot (WITH JITTERS)
  p1 <- ggplot(Data2, aes(x=interaction(AgeGroup_New, gender), y=IR_COVID)) +
    geom_point(aes(col=databaseName, shape=gender),size=2, position = position_jitter(w = 0.2, h = 0)) +
    scale_y_continuous(trans='log10', labels=comma_format(accuracy=1)) +
    facet_wrap(~outcomeName) +
    theme_light () +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
          panel.grid.minor = element_blank()) +
    theme(strip.text = element_text(face="bold", size=10, colour="black"),
          strip.background = element_rect(fill="lavenderblush", colour="black",size=0.5)) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    theme(legend.title=element_blank(),
          legend.text=element_text(size=10),
          legend.position = "bottom") +
    guides(color=guide_legend(nrow=3, byrow=TRUE)) +
    scale_color_viridis(discrete=TRUE, option="plasma") +
    labs(y="Incidence rate per 100,000 person-years",
         x="Age groups (years) and sex")+#,
         #title="Age and sex stratified incidence rates for 16 adverse events of special interest by database") +
    geom_vline(xintercept = 8.5, linetype="dashed", color = "red", size=1)
  plot(p1)

  # Plot (WITHOUT JITTERS)
  # p1 <- ggplot(Data2, aes(x=interaction(AgeGroup_New, gender), y=IR_COVID)) +
  #   geom_point(aes(col=databaseName, shape=gender),size=2) +
  #   scale_y_continuous(trans='log10', labels=comma_format(accuracy=1)) +
  #   facet_wrap(~outcomeName) +
  #   theme_light () +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1),
  #         panel.grid.minor = element_blank()) +
  #   theme(strip.text = element_text(face="bold", size=10, colour="black"),
  #         strip.background = element_rect(fill="lavenderblush", colour="black",size=0.5)) +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   theme(legend.title=element_blank(),
  #         legend.text=element_text(size=8),
  #         legend.position = "bottom") +
  #   guides(color=guide_legend(nrow=3, byrow=TRUE)) +
  #   scale_color_viridis(discrete=TRUE, option="plasma") +
  #   labs(y="Incidence rate per 100,000 person-years",
  #        x="Age groups (years)",
  #        title="Age and sex stratified incidence rates for 16 adverse events of special interest by database") +
  #   geom_vline(xintercept = 8.5, linetype="dashed", color = "red", size=1)
  # plot(p1)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName, p1, width = 19, height = 11, dpi = 400)
  invisible(plot)

  ##################
  #### FIGURE 2 ####
  ##################
  fileName2 <- paste0(resultsFolder,"/COVID_SIR_VS_GENERAL_SIR.pdf")

  p2 <- ggplot(Data2, aes(x=IR_POP, y=IR_COVID)) +
    geom_point(aes(col=databaseName, shape=gender),size=2) +
    facet_wrap(~outcomeName) +
    geom_abline(intercept = 0.1, size = 0.5) +
    scale_y_continuous(trans='log10', labels=comma_format(accuracy=1)) +
    scale_x_continuous(trans='log10', labels=comma_format(accuracy=1)) +
    coord_cartesian(ylim = c(1, 100000)) +
    coord_cartesian(xlim = c(1, 100000)) +
    theme_light () +
    theme(strip.text = element_text(face="bold", size=10, colour="black"),
          strip.background = element_rect(fill="azure1", colour="black",size=0.5)) +
    theme(legend.title=element_blank(),
          legend.text=element_text(size=10),
          legend.position = "bottom") +
    guides(color=guide_legend(nrow=3, byrow=TRUE)) +
    scale_color_viridis(discrete=TRUE, option="plasma") +
    labs(y="Weighted incidence rate per 100,000 person-years (COVID-19 population)",
         x="Weighted incidence rate per 100,000 person-years (General population)")
  plot(p2)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName2, p2, width = 19, height = 11, dpi = 400)
  invisible(plot)
}
