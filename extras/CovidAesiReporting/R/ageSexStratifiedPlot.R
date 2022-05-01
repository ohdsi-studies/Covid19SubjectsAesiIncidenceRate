ageSexStratifiedPlot <- function(resultsFolder, irFolder){

  library("ggplot2")
  library("ggh4x")
  library("wesanderson")
  library("scales")
  library("ggbrace")

  fileName <- paste0(resultsFolder,"/AGE_SEX_STRATIFIED_IR.png")

  # Import data
  Data1 <- read.csv(paste0(irFolder,"/incidenceAnalysisCensored.csv"))
  names (Data1)
  table (Data1$targetName)
  table (Data1$timeAtRiskId)

  # Variables of interest
  # y-axis: incidenceRateP100py
  # x-axis: ageGroup, gender by databaseName
  # Outcome: outcomeName
  # Target group: Earliest COVID-19 Event (positive test OR diagnosis)
  # TAR: 6 (or 90 days)
  Data2 <- Data1[ which(Data1$targetName=='Earliest COVID-19 Event (positive test OR diagnosis)' & Data1$timeAtRiskId ==6), ]
  Data3 <- subset(Data2, select=c(1,3,4,13,24:26))
  names (Data3)

  # Remove the combined age groups & 0-5yo, 6-17yo & combined sex
  Data4 <- subset(Data3, !(Data3$ageGroup == "ALL" | Data3$ageGroup=="000-005" | Data3$ageGroup=="006-017"| Data3$gender=="M&F"))
  table (Data4$gender)
  table (Data4$ageGroup)

  # Rename age groups
  Data4[grep ("000-004", Data4$ageGroup), "AgeGroup_New"] <- "0-4"
  Data4[grep ("005-011", Data4$ageGroup), "AgeGroup_New"] <- "5-11"
  Data4[grep ("012-017", Data4$ageGroup), "AgeGroup_New"] <- "12-17"
  Data4[grep ("018-034", Data4$ageGroup), "AgeGroup_New"] <- "18-34"
  Data4[grep ("035-054", Data4$ageGroup), "AgeGroup_New"] <- "35-54"
  Data4[grep ("055-064", Data4$ageGroup), "AgeGroup_New"] <- "55-64"
  Data4[grep ("065-074", Data4$ageGroup), "AgeGroup_New"] <- "65-74"
  Data4[grep ("075-084", Data4$ageGroup), "AgeGroup_New"] <- "75-84"
  Data4[grep ("085-11", Data4$ageGroup), "AgeGroup_New"] <- ">85"

  Data4$AgeGroup_New <- factor(Data4$AgeGroup_New, levels = c("0-4", "5-11", "12-17", "18-34", "35-54", "55-64", "65-74", "75-84", ">85"))

  # PLOT 1
  # Create ggplot
  p1 <- ggplot(Data4, aes(x=interaction(AgeGroup_New, gender), y=incidenceRateP100py)) +
    geom_point(aes(col=databaseName, shape=gender)) +
    scale_y_continuous(trans='log10', labels=comma_format(accuracy=0.1)) +
    facet_wrap(~outcomeName) +
    theme_light () +
    #scale_color_brewer(palette = "Darjeeling1") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size=11),
          panel.grid.minor = element_blank()) +
    theme(strip.text = element_text(face="bold", size=12, colour="black"),
          strip.background = element_rect(fill="lavenderblush", colour="black",size=0.5)) +
    theme(legend.title=element_blank(),
          legend.text=element_text(size=12),
          legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 1)) +
    guides(color=guide_legend(nrow=3, byrow=TRUE))+
    labs(y="Incidence rate per 100 person-years",
         x="Age groups (years)")
  plot(p1)

  if (!is.null(fileName))
    ggplot2::ggsave(fileName, p1, width = 19, height = 13, dpi = 400)
  invisible(plot)
}
