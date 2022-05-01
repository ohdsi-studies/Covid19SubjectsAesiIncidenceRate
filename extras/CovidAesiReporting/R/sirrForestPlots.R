sirrForestPlots <- function(summaryIR, metaAnalysisIR,aesi){

  #Variables
  fileName <- paste0(resultsFolder,"/ForestPlot_",aesi,".png")
  summaryIR <- summaryIR[summaryIR$outcomeName ==aesi, ]
  metaAnalysisIR <- metaAnalysisIR[metaAnalysisIR$outcomeName ==aesi,]
  labels <- c(summaryIR$databaseName)

  xLabel = "Standardized Relative Risk (SRR)"
  xLabelSmall = "SRR"
  limits = c(0.1, 30)
  summaryLabel = "Summary"

  #assemble data object (header, DB, MA)
  d1 <- data.frame(logRr = -100,
                   logLb95Ci = -100,
                   logUb95Ci = -100,
                   type = "header",
                   name = "Source")

  # ggplot puts whisker for infinite values, but not large values:
  d2 <- data.frame(logRr = summaryIR$logSIR,
                   logLb95Ci = log(summaryIR$SIRLB),
                   logUb95Ci = log(summaryIR$SIRUB),
                   type = "db",
                   name = summaryIR$databaseName)
  d2$name <- labels

  d3 <- data.frame(logRr = log(metaAnalysisIR$SIR),
                   logLb95Ci = log(metaAnalysisIR$SIRLB),
                   logUb95Ci = log(metaAnalysisIR$SIRUB),
                   type = "ma",
                   name = summaryLabel)

  d <- rbind(d1,d2,d3)
  d$name <- factor(d$name, levels = c(d3$name, rev(as.character(labels)), "Source"))

  #start building plot up
  plotD <- d
  plotD$logLb95Ci[is.infinite(plotD$logLb95Ci)] <- -10
  plotD$logUb95Ci[is.infinite(plotD$logUb95Ci)] <- 10

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10,20,30)
  p <- ggplot2::ggplot(plotD, ggplot2::aes(x = exp(.data$logRr),
                                           y = .data$name,
                                           xmin = exp(.data$logLb95Ci),
                                           xmax = exp(.data$logUb95Ci))) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 1, size = 0.5) +
    ggplot2::geom_errorbarh(height = 0.15) +
    ggplot2::geom_point(size = 3, shape = 23, ggplot2::aes(fill = .data$type)) +
    ggplot2::scale_fill_manual(values = c("#000000", "#000000", "#FFFFFF")) +
    ggplot2::scale_x_continuous(xLabel, trans = "log10", breaks = breaks, labels = breaks) +
    ggplot2::coord_cartesian(xlim = limits) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines"))

  d$logLb95Ci[is.infinite(d$logLb95Ci)] <- NA
  d$logUb95Ci[is.infinite(d$logUb95Ci)] <- NA
  labels <- sprintf("%0.2f (%0.2f - %0.2f)", exp(d$logRr), exp(d$logLb95Ci), exp(d$logUb95Ci))
  labels <- gsub("NA", "", labels)
  labels <- data.frame(y = rep(d$name, 2),
                       x = rep(1:2, each = nrow(d)),
                       label = c(as.character(d$name), labels),
                       stringsAsFactors = FALSE)
  labels$label[nrow(d) + 1] <- paste(xLabelSmall, "(95% CI)")
  data_table <- ggplot2::ggplot(labels, ggplot2::aes(x = .data$x,
                                                     y = .data$y,
                                                     label = .data$label)) +
    ggplot2::geom_text(size = 4, hjust = 0, vjust = 0.5) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(colour = "white"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_line(colour = "white"),
                   plot.margin = grid::unit(c(0, 0, 0.1, 0), "lines")) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_cartesian(xlim = c(1, 3))

  plot <- gridExtra::grid.arrange(data_table, p, ncol = 2)

  #save out
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 8, height = 1 + nrow(summaryIR) * 0.3, dpi = 400)
  invisible(plot)
}
