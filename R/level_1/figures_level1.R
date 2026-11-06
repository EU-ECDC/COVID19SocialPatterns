#' Generate diagnostic plots and checks for a Stan model fit
#'
#' This function loads a Stan model fit for a specified country and generates
#' a comprehensive set of diagnostic plots to assess model convergence and parameter behavior.
#'
#' @param country A character string specifying the country to analyze
#' @return A list containing the following diagnostic elements:
#'   \item{hmc_diagnosis}{Results from HMC diagnostic checks}
#'   \item{traceplot_lp}{Traceplot of log posterior and hyperparameters}
#'   \item{traceplot_beta}{Traceplot of beta parameters}
#'   \item{stan_dens_lp}{Density plots for log posterior and hyperparameters}
#'   \item{stan_dens_beta}{Density plots for beta parameters}
#'   \item{pairs_plot}{Pairs plot showing parameter correlations}
diagnostics <- function(country, n_age_groups) {
  
  loaded_fit <- load_fit(country, n_age_groups)
  
  hmc_diagnosis <- check_hmc_diagnostics(loaded_fit)
  
  traceplot_lp <- traceplot(loaded_fit, pars = c("lp__", "tau1", "tau2", "tau3", "tau4", "sigma"), inc_warmup = TRUE, nrow = 2)
  traceplot_beta <- traceplot(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), inc_warmup = TRUE, nrow = 4)
  
  stan_dens_lp <- stan_dens(loaded_fit, pars = c("lp__", "tau1", "tau2", "tau3", "tau4", "sigma"), separate_chains = TRUE)
  stan_dens_beta <- stan_dens(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), separate_chains = TRUE, nrow = 4)
  
  pairs_plot <- pairs(loaded_fit, pars = c("beta1", "beta2", "beta3", "beta4"), labels = c("beta1", "beta2", "beta3", "beta4"),
                      cex.labels = 1.5, font.labels = 9, condition = "accept_stat__")
  
  return(list(hmc_diagnosis = hmc_diagnosis,
              traceplot_lp = traceplot_lp,
              traceplot_beta = traceplot_beta,
              stan_dens_lp = stan_dens_lp,
              stan_dens_beta = stan_dens_beta,
              pairs_plot = pairs_plot))
}

#' Adjust theme elements based on plot position in a grid
#'
#' This helper function modifies ggplot theme elements to create cleaner
#' multi-panel plots with shared axes. It selectively shows/hides axis elements
#' based on the plot's position in the grid.
#'
#' @param plot A ggplot object to be modified
#' @param pos Integer indicating position in the grid
#' @param grid_cols Integer indicating number of columns in the grid
#' @param grid_rows Integer indicating number of rows in the grid  
#' @return A ggplot object with modified theme elements
adjust_theme <- function(plot, pos, grid_cols, grid_rows) {
  # Calculate row and column of current position
  row <- ceiling(pos / grid_cols)
  col <- ((pos - 1) %% grid_cols) + 1
  
  # Determine if this is a bottom row or left column position
  is_bottom_row <- (row == grid_rows)
  is_left_col <- (col == 1)
  
  theme_settings <- list(
    axis.title.x = if (is_bottom_row) element_text() else element_blank(),
    axis.text.x = if (is_bottom_row) element_text() else element_blank(),
    axis.ticks.x = if (is_bottom_row) element_line() else element_blank(),
    axis.title.y = if (is_left_col) element_text() else element_blank(),
    axis.text.y = if (is_left_col) element_text() else element_blank(),
    axis.ticks.y = if (is_left_col) element_line() else element_blank()
  )
  plot + theme(theme_settings)
}

# Wraps a ggarrange figure with row/column strip labels like a contact matrix
add_matrix_labels <- function(figure, age_group_labels) {
  n <- length(age_group_labels)
  
  # Column labels (top) = age group of contactee (j)
  col_labels <- wrap_elements(
    grid::textGrob(paste(age_group_labels, collapse = "          "),
                   gp = grid::gpar(fontsize = 14, fontface = "bold"))
  )
  
  # Row labels (left) = age group of contactor (i), rotated
  row_label_grobs <- lapply(age_group_labels, function(lbl) {
    wrap_elements(grid::textGrob(lbl, rot = 90,
                                 gp = grid::gpar(fontsize = 14, fontface = "bold")))
  })
  
  # Build row label column using patchwork
  row_col <- Reduce(`/`, row_label_grobs) + 
    plot_layout(heights = rep(1, n))
  
  # Combine: row labels | figure, with column label on top
  combined <- (row_col | wrap_elements(figure)) +
    plot_layout(widths = c(0.05, 1))
  
  combined <- col_labels / combined +
    plot_layout(heights = c(0.04, 1))
  
  return(combined)
}

#' Create plots of the fitted beta parameters over time
#'
#' This function visualizes the temporal trends in the beta parameters 
#' (contact coefficients) for different contact settings (Home, Work, School, Other).
#' It creates a 2x2 grid of plots showing posterior medians and 90% credible intervals.
#'
#' @param country A character string specifying the country to analyze
#' @return A ggplot object containing a 2x2 grid of plots for beta parameters
fitted_beta_interp <- function(country, label, n_age_groups) {
  
  # Get data for all percentiles
  tvem_beta_50 <- fitted_beta_coefficients(country, "50", n_age_groups)[,-c(2,4,6,8)]
  tvem_beta_w_50 <- fitted_beta_coefficients(country, "50", n_age_groups)[,-c(3,5,7,9)]
  
  tvem_beta_2.5 <- fitted_beta_coefficients(country, "2.5", n_age_groups)[,-c(2,4,6,8)]
  tvem_beta_w_2.5 <- fitted_beta_coefficients(country, "2.5", n_age_groups)[,-c(3,5,7,9)]
  
  tvem_beta_97.5 <- fitted_beta_coefficients(country, "97.5", n_age_groups)[,-c(2,4,6,8)]
  tvem_beta_w_97.5 <- fitted_beta_coefficients(country, "97.5", n_age_groups)[,-c(3,5,7,9)]
  
  # Extract time and create data frames
  time_ext <- tvem_beta_50[,1]
  
  # Interpolated data (continuous)
  beta_tvem_50 <- data.frame(time = time_ext, tvem_beta = tvem_beta_50[,label + 1])
  colnames(beta_tvem_50) <- c("time", "q50")
  beta_tvem_2.5 <- data.frame(time = time_ext, tvem_beta = tvem_beta_2.5[,label + 1])
  colnames(beta_tvem_2.5) <- c("time", "q5")
  beta_tvem_97.5 <- data.frame(time = time_ext, tvem_beta = tvem_beta_97.5[,label + 1])
  colnames(beta_tvem_97.5) <- c("time", "q95")
  
  # Wave data (discrete points)
  beta_tvem_w_50 <- data.frame(time = time_ext, tvem_beta_w = tvem_beta_w_50[,label + 1])
  colnames(beta_tvem_w_50) <- c("time", "q50_w")
  beta_tvem_w_2.5 <- data.frame(time = time_ext, tvem_beta_w = tvem_beta_w_2.5[,label + 1])
  colnames(beta_tvem_w_2.5) <- c("time", "q5_w")
  beta_tvem_w_97.5 <- data.frame(time = time_ext, tvem_beta_w = tvem_beta_w_97.5[,label + 1])
  colnames(beta_tvem_w_97.5) <- c("time", "q95_w")
  
  # Merge all data
  merged_df <- merge(beta_tvem_50, beta_tvem_2.5, by = "time", all.x = TRUE)
  merged_df <- merge(beta_tvem_97.5, merged_df, by = "time", all.x = TRUE)
  merged_df <- merge(beta_tvem_w_50, merged_df, by = "time", all.x = TRUE)
  merged_df <- merge(beta_tvem_w_2.5, merged_df, by = "time", all.x = TRUE)
  merged_df <- merge(beta_tvem_w_97.5, merged_df, by = "time", all.x = TRUE)
  
  names_beta_fig <- c("Home", "Work", "School", "Other")
  
  date_break_interval <- if(country == "BE"|country == "NL"|country == "FI"|country == "LT") "4 weeks" else "1 week"
  
  g <- ggplot(merged_df, aes(x = as.Date(time))) +
    geom_ribbon(aes(ymin = as.numeric(q5), ymax = as.numeric(q95), fill = "Interpolated 95% CI", group = 1), alpha = 0.2) +
    geom_line(aes(y = as.numeric(q50), color = "Interpolated Median", group = 1), size = 1) +
    geom_errorbar(
      aes(ymin = as.numeric(q5_w), ymax = as.numeric(q95_w), color = "Wave 95% CI"), 
      width = 5, size = 1, alpha = 0.7) +
    geom_point(aes(y = as.numeric(q50_w), color = "Wave Median"), size = 2, shape = 16, stroke = 0) +
    scale_color_manual(name = '', values = c("Interpolated Median" = "#4A90E2", "Wave Median" = 'black', "Wave 95% CI" = "#999999")) +
    scale_fill_manual(name = '', values = c("Interpolated 95% CI" = "#4A90E2")) +
    labs(x = "time", y = paste("beta", names_beta_fig[label])) +
    scale_x_date(date_breaks = date_break_interval, date_labels = "%d %b %Y") +
    scale_y_continuous(n.breaks = 8) +
    theme(
      text = element_text(size = 32),
      axis.text = element_text(size = 30),
      axis.title = element_text(size = 30),
      title = element_text(size = 32),
      # Reduce legend text size significantly
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      # Make legend key smaller to save space
      legend.key.size = unit(0.8, "lines"),
      # Control legend margins
      legend.margin = margin(t = 5, r = 5, b = 5, l = 5),
      axis.text.x = element_text(size = 16, angle = 45, hjust = 1)
    )
  return(g)
}

#' Plot All TVEM Beta Coefficients in a 2x2 Grid
#'
#' Creates a 2x2 grid showing all four beta coefficients with interpolated lines/ribbons
#' and wave-specific points/error bars.
#'
#' @param country Character string indicating the country to analyze
#' @param n_age_groups Integer. Number of age groups.
#' @return A ggplot object containing a 2x2 grid of TVEM beta plots
fitted_beta_interp_all <- function(country, n_age_groups) {
  
  plot_list <- list()
  
  for (i in 1:4) {
    plot_list[[i]] <- fitted_beta_interp(country, i, n_age_groups) +
      theme(
        axis.text.x = if(i %in% c(1, 2)) element_blank() else element_text(angle = 45, hjust = 1),
        axis.title.x = if(i %in% c(1, 2)) element_blank() else element_text(),
        # Further reduce legend size for the combined plot
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.6, "lines")
      )
  }
  
  # Combine plots into a 2x2 grid
  figure <- ggarrange(plotlist = plot_list, 
                      labels = "AUTO",
                      ncol = 2, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")
  
  return(figure)
}

#' Generate age group pair labels for contact matrices
#'
#' This helper function creates labels for all possible age group pairs (from -> to).
#'
#' @param age_group_labels Character vector of age group labels
#' @return Character vector of contact pair labels (e.g., "0-12 to 18-25")
generate_age_pair_labels <- function(age_group_labels) {
  n_groups <- length(age_group_labels)
  pair_labels <- character(n_groups^2)
  
  idx <- 1
  for (i in 1:n_groups) {
    for (j in 1:n_groups) {
      pair_labels[idx] <- paste(age_group_labels[i], "to", age_group_labels[j])
      idx <- idx + 1
    }
  }
  
  return(pair_labels)
}

#' Determine optimal grid layout for a given number of plots
#'
#' This helper function calculates the best rectangular grid layout for visualization.
#'
#' @param n_plots Integer number of plots to arrange
#' @return List with ncol and nrow for the grid layout
get_optimal_grid <- function(n_plots) {
  if (n_plots <= 4) {
    return(list(ncol = 2, nrow = 2))
  } else if (n_plots <= 9) {
    return(list(ncol = 3, nrow = 3))
  } else if (n_plots <= 16) {
    return(list(ncol = 4, nrow = 4))
  } else if (n_plots <= 25) {
    return(list(ncol = 5, nrow = 5))
  } else if (n_plots <= 36) {
    return(list(ncol = 6, nrow = 6))
  } else {
    # For very large numbers, use a rectangular layout
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots / ncol)
    return(list(ncol = ncol, nrow = nrow))
  }
}

#' Create plots of fitted contact rates between age groups over time
#'
#' This function visualizes the temporal trends in contact rates between different
#' age groups. It generates a grid of plots showing posterior predictions (median and
#' 90% credible intervals) alongside observed CoMix data points for each age group pair.
#'
#' @param country A character string specifying the country to analyze
#' @param age_group_labels Character vector. Labels for age groups. If NULL, will try to infer from data.
#' @return A ggplot object containing a grid of plots for contacts between age groups
fitted_contacts <- function(country, age_group_labels = NULL, n_age_groups) {
  # Load the Stan model fit for the specified country
  loaded_fit <- load_fit(country, n_age_groups)
  
  # Create a sequence representing time points/waves
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  time <- c(1:nrow(comix_matrix))
  num_rows <- nrow(comix_matrix)
  num_cols <- ncol(comix_matrix)
  
  # Infer age group structure if not provided
  if (is.null(age_group_labels)) {
    n_age_groups <- get_n_age_groups(country)
    if (is.null(n_age_groups)) {
      # Fallback: try to infer from matrix dimensions
      n_age_groups <- sqrt(num_cols)
      if (n_age_groups != round(n_age_groups)) {
        stop("Cannot determine number of age groups. Please provide age_group_labels parameter.")
      }
      n_age_groups <- round(n_age_groups)
    }
    age_group_labels <- get_age_group_labels(n_age_groups)
  }
  
  # # Generate age group pair labels
  # y_labels <- generate_age_pair_labels(age_group_labels)
  # 
  # # Validate that we have the right number of labels
  # if (length(y_labels) != num_cols) {
  #   warning(paste("Generated", length(y_labels), "age pair labels but data has", num_cols, "columns."))
  # }
  
  grid_layout <- get_optimal_grid(num_cols)
  n_groups    <- grid_layout$ncol  # square grid
  
  # Create parameter names matrix for Stan output
  param_names <- matrix("", nrow = num_rows, ncol = num_cols)
  for (j in 1:num_cols) {
    for (i in 1:num_rows) {
      param_names[i, j] <- sprintf("y_pred[%d,%d]", i, j)
    }
  }
  
  #' Helper function to create a plot for a specific contact type
  #'
  #' @param j Column index in the contact matrix representing a specific contact type
  #' @return A ggplot object for the specified contact type
  create_plot <- function(j) {
    # Determine row/col position in the grid
    row_i        <- ceiling(j / n_groups)
    is_bottom    <- row_i == n_groups
    
    y_pred <- cbind(
      as.data.frame(summary(loaded_fit, pars = param_names[, j], 
                            probs = c(0.05, 0.5, 0.95))$summary),
      time, 
      unlist(comix_matrix[, j])
    )
    colnames(y_pred) <- make.names(colnames(y_pred)) # Remove % in the column names
    
    # ggplot(y_pred, aes(x = time)) +
    #   geom_ribbon(aes(ymin = X5., ymax = X95., fill = "95% CI"), alpha = 0.2) +
    #   geom_line(aes(x = time, y = X50., color = "Median"), size = 1) +
    #   geom_point(aes(y = unlist(comix_matrix[, j]), color = "CoMix"), size = 2, shape = 16, stroke = 0) +
    #   scale_color_manual(name = '', values = c("Median" = "#33a02c", "CoMix" = 'black')) +
    #   scale_fill_manual(name = '', values = c("95% CI" = "#33a02c")) +
    #   labs(x = "Wave", y = if(j <= length(y_labels)) y_labels[j] else paste("Contact", j)) +
    #   theme(
    #     text = element_text(size = 14),
    #     axis.text = element_text(size = 12),
    #     axis.title = element_text(size = 14),
    #     title = element_text(size = 16),
    #     legend.text = element_text(size = 12),
    #     legend.title = element_text(size = 14)
    #   )
    
    ggplot(y_pred, aes(x = time)) +
      geom_ribbon(aes(ymin = X5., ymax = X95., fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = X50., color = "Median"), size = 1) +
      geom_point(aes(y = unlist(comix_matrix[, j]), color = "CoMix"), size = 2, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#33a02c", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#33a02c")) +
      labs(x = if (is_bottom) "Wave" else "", y = "") +
      theme(
        text        = element_text(size = 14),
        axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14),
        title       = element_text(size = 16),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 12, margin = margin(r = 2)),
        axis.title.x = if (is_bottom) element_text() else element_blank(),
        axis.text.x  = if (is_bottom) element_text(size = 12) else element_blank(),
        axis.ticks.x = if (is_bottom) element_line() else element_blank()
      )
  }
  
  # # Determine optimal grid layout
  # grid_layout <- get_optimal_grid(num_cols)
  # 
  # Create all plots
  plot_list <- lapply(1:num_cols, create_plot)
  
  # Arrange plots in optimal grid
  # figure2 <- ggarrange(plotlist = plot_list,
  #                      labels = "AUTO",
  #                      ncol = grid_layout$ncol, 
  #                      nrow = grid_layout$nrow,
  #                      common.legend = TRUE,
  #                      legend = "bottom")
  
  # figure_inner <- ggarrange(
  #   plotlist     = plot_list,
  #   labels       = NULL,        # strip AUTO labels — matrix strips replace them
  #   ncol         = grid_layout$ncol,
  #   nrow         = grid_layout$nrow,
  #   common.legend = TRUE,
  #   legend        = "bottom"
  # )
  
  figure_inner <- (wrap_plots(plot_list, ncol = grid_layout$ncol, nrow = grid_layout$nrow) +
                     plot_layout(guides = "collect", axes = "collect")) &
    theme(legend.position = "bottom")
  
  col_label_grobs <- lapply(age_group_labels, function(lbl) {
    wrap_elements(grid::textGrob(lbl, gp = grid::gpar(fontsize = 13, fontface = "bold"))) +
      theme(plot.margin = margin(0, 0, 0, 0))
  })
  col_strip <- Reduce(`+`, col_label_grobs) + plot_layout(ncol = n_groups)
  
  row_label_grobs <- lapply(age_group_labels, function(lbl) {
    wrap_elements(grid::textGrob(lbl, rot = 90, gp = grid::gpar(fontsize = 13, fontface = "bold"))) +
      theme(plot.margin = margin(0, 0, 0, 0))
  })
  row_strip <- Reduce(`/`, row_label_grobs) + plot_layout(nrow = n_groups)
  
  main_with_rows <- (row_strip | figure_inner) +
    plot_layout(widths = c(0.025, 1)) &
    theme(plot.margin = margin(1, 1, 1, 1))
  
  figure2 <- (col_strip / main_with_rows) +
    plot_layout(heights = c(0.02, 1)) &
    theme(plot.margin = margin(1, 1, 1, 1))
  
  return(figure2)
}