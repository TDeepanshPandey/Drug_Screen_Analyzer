# custom_plot_functions.R

customPlotGR <- function(fitData, metric = "GR", experiments = "all", points = TRUE, curves = TRUE, plotly = FALSE, line_thickness = 1.5, legend_size = 12, point_size = 3, show_mean_only = FALSE) {
  if (!points & !curves) {
    stop("You must show either points, curves, or both.")
  }
  
  # Extract the necessary data from fitData
  data <- S4Vectors::metadata(fitData)[[1]]
  parameterTable <- cbind(as.data.frame(SummarizedExperiment::colData(fitData)), 
                          t(SummarizedExperiment::assay(fitData)))
  groupingVariables <- S4Vectors::metadata(fitData)[[2]]
  
  data$log10_concentration <- log10(data$concentration)
  
  # Create experiment labels
  experimentNew <- apply(data[, groupingVariables, drop = FALSE], 1, function(x) paste(x, collapse = " "))
  data$experiment <- as.factor(if (length(groupingVariables) > 0) experimentNew else "All Data")
  
  if (!identical(experiments, "all")) {
    parameterTable <- parameterTable[parameterTable$experiment %in% experiments, ]
    data <- data[data$experiment %in% experiments, ]
  }
  
  # Calculate mean values if show_mean_only is TRUE
  if (show_mean_only && points) {
    # For GR values
    mean_data <- aggregate(
      cbind(GRvalue, rel_cell_count) ~ concentration + experiment, 
      data = data, 
      FUN = mean, 
      na.rm = TRUE
    )
    mean_data$log10_concentration <- log10(mean_data$concentration)
    
    # Replace original data with mean data for plotting
    plot_data <- mean_data
  } else {
    plot_data <- data
  }
  
  exps <- unique(parameterTable$experiment)
  min_conc <- min(data$concentration, na.rm = TRUE)
  max_conc <- max(data$concentration, na.rm = TRUE)
  len <- (log10(max_conc) - log10(min_conc)) * 100
  Concentration <- 10^(seq(log10(min_conc) - 1, log10(max_conc) + 1, length.out = len))
  
  curve_data_all <- NULL
  
  for (exp in exps) {
    row <- which(parameterTable$experiment == exp)
    
    if (metric == "GR") {
      logistic_3u <- function(c) {
        parameterTable$GRinf[row] + (1 - parameterTable$GRinf[row]) /
          (1 + (c / parameterTable$GEC50[row])^parameterTable$h_GR[row])
      }
    } else if (metric %in% c("rel_cell", "IC")) {
      logistic_3u <- function(c) {
        parameterTable$Einf[row] + (1 - parameterTable$Einf[row]) /
          (1 + (c / parameterTable$EC50[row])^parameterTable$h[row])
      }
    }
    
    curve_data <- as.data.frame(Concentration)
    colnames(curve_data) <- "Concentration"
    
    if (metric == "GR") {
      GR <- if (parameterTable$fit_GR[row] == "sigmoid") {
        apply(curve_data, 1, logistic_3u)
      } else {
        parameterTable$flat_fit_GR[row]
      }
      curve_data$GR <- GR
    } else if (metric %in% c("rel_cell", "IC")) {
      rel_cell_count <- if (parameterTable$fit_rel_cell[row] == "sigmoid") {
        apply(curve_data, 1, logistic_3u)
      } else {
        parameterTable$flat_fit_rel_cell[row]
      }
      curve_data$rel_cell_count <- rel_cell_count
    }
    
    curve_data$experiment <- exp
    
    if (is.null(curve_data_all)) {
      curve_data_all <- curve_data
    } else {
      curve_data_all <- rbind(curve_data_all, curve_data)
    }
  }
  
  curve_data_all <- merge(curve_data_all, parameterTable[, c(groupingVariables, "experiment")])
  curve_data_all$experiment <- as.factor(curve_data_all$experiment)
  
  if (metric == "GR") {
    p <- ggplot2::ggplot()
    
    if (curves) {
      p <- p + ggplot2::geom_line(data = curve_data_all, 
                                  ggplot2::aes(x = log10(Concentration), y = GR, colour = experiment),
                                  size = line_thickness)
    }
    if (points) {
      p <- p + ggplot2::geom_point(data = plot_data, 
                                  ggplot2::aes(x = log10_concentration, y = GRvalue, colour = experiment),
                                  size = point_size)
      
      # Add error bars if using mean values
      if (show_mean_only) {
        p <- p + ggplot2::geom_point(data = plot_data, 
                                    ggplot2::aes(x = log10_concentration, y = GRvalue, colour = experiment),
                                    size = point_size + 1, shape = 16)  # Changed to circle (shape 16) for mean points
      }
    }
    
    p <- p + ggplot2::coord_cartesian(xlim = c(log10(min_conc) - 0.1, log10(max_conc) + 0.1), ylim = c(-1, 1.5), expand = TRUE) +
      ggplot2::ggtitle("Concentration vs. GR values") +
      ggplot2::xlab("Concentration (log10 scale)") +
      ggplot2::ylab("GR value") +
      ggplot2::labs(colour = "") +
      ggplot2::geom_hline(yintercept = c(1, 0.5, 0, -1), size = 0.25) +
      ggplot2::theme(legend.text = ggplot2::element_text(size = legend_size),
                    legend.title = ggplot2::element_text(size = legend_size),
                    legend.key.size = ggplot2::unit(1.2, "lines"))
  } else if (metric %in% c("rel_cell", "IC")) {
    p <- ggplot2::ggplot()
    
    if (curves) {
      p <- p + ggplot2::geom_line(data = curve_data_all, 
                                  ggplot2::aes(x = log10(Concentration), y = rel_cell_count, colour = experiment),
                                  size = line_thickness)
    }
    if (points) {
      p <- p + ggplot2::geom_point(data = plot_data, 
                                  ggplot2::aes(x = log10_concentration, y = rel_cell_count, colour = experiment),
                                  size = point_size)
      
      # Add error bars if using mean values
      if (show_mean_only) {
        p <- p + ggplot2::geom_point(data = plot_data, 
                                    ggplot2::aes(x = log10_concentration, y = rel_cell_count, colour = experiment),
                                    size = point_size + 1, shape = 16)  # Changed to circle (shape 16) for mean points
      }
    }
    
    p <- p + ggplot2::coord_cartesian(xlim = c(log10(min_conc) - 0.1, log10(max_conc) + 0.1), ylim = c(0, 1.5), expand = TRUE) +
      ggplot2::ggtitle("Concentration vs. Relative cell count") +
      ggplot2::xlab("Concentration (log10 scale)") +
      ggplot2::ylab("Relative cell count") +
      ggplot2::labs(colour = "") +
      ggplot2::geom_hline(yintercept = c(1, 0.5, 0), size = 0.25) +
      ggplot2::theme(legend.text = ggplot2::element_text(size = legend_size),
                    legend.title = ggplot2::element_text(size = legend_size),
                    legend.key.size = ggplot2::unit(1.2, "lines"))
  }
  
  if (plotly) {
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}
