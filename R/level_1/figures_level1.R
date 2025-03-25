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
diagnostics <- function(country) {
  
  loaded_fit <- load_fit(country)
  
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
#' @param pos Integer indicating position in the grid: 1=top-left, 2=top-right, 
#'   3=bottom-left, 4=bottom-right
#' @return A ggplot object with modified theme elements
adjust_theme <- function(plot, pos) {
  theme_settings <- list(
    axis.title.x = if (pos %in% c(3, 4)) element_text() else element_blank(),
    axis.text.x = if (pos %in% c(3, 4)) element_text() else element_blank(),
    axis.ticks.x = if (pos %in% c(3, 4)) element_line() else element_blank(),
    axis.title.y = if (pos %in% c(1, 3)) element_text() else element_blank(),
    axis.text.y = if (pos %in% c(1, 3)) element_text() else element_blank(),
    axis.ticks.y = if (pos %in% c(1, 3)) element_line() else element_blank()
  )
  plot + theme(theme_settings)
}



#' Create plots of the fitted beta parameters over time
#'
#' This function visualizes the temporal trends in the beta parameters 
#' (contact coefficients) for different contact settings (Home, Work, School, Other).
#' It creates a 2x2 grid of plots showing posterior medians and 90% credible intervals.
#'
#' @param country A character string specifying the country to analyze
#' @return A ggplot object containing a 2x2 grid of plots for beta parameters
fitted_beta <- function(country) {
  # Load the Stan model fit for the specified country
  loaded_fit <- load_fit(country)
  
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  beta_labels <- c("Home", "Work", "School", "Other")
  
  plot_list <- list()
  # Create a plot for each beta parameter
  for (i in seq_along(beta_labels)) {
    # Extract posterior summary statistics (median and 90% CI) for this beta
    beta <- as.data.frame(summary(
      loaded_fit, pars = paste0("beta", i), probs = c(0.05, 0.5, 0.95))$summary)
    colnames(beta) <- make.names(colnames(beta))
    
    plot <- ggplot(beta, mapping = aes(x = c(1:nrow(comix_matrix)))) +
      geom_ribbon(aes(ymin = X5., ymax = X95.), alpha = 0.35) +
      geom_line(mapping = aes(x = c(1:nrow(comix_matrix)), y = X50.)) + 
      labs(x = "Wave", y = paste("beta", beta_labels[i])) +
      theme_minimal(base_size = 14) +  # Increase base font size
      theme(plot.margin = unit(c(1, 1, 1, 1), "lines"))
    # Adjust axes based on plot position (1-4)
    plot_list[[i]] <- adjust_theme(plot, i)
  }
  
  figure1 <- ggarrange(plotlist = plot_list, 
                       labels = "AUTO",
                       ncol = 2, nrow = 2)
  
  return(figure1)
}



#' Create plots of fitted contact rates between age groups over time
#'
#' This function visualizes the temporal trends in contact rates between different
#' age groups. It generates a grid of plots showing posterior predictions (median and
#' 90% credible intervals) alongside observed CoMix data points for each age group pair.
#'
#' @param country A character string specifying the country to analyze
#' @return A ggplot object containing a grid of plots for contacts between age groups
fitted_contacts <- function(country){
  # Load the Stan model fit for the specified country
  loaded_fit <- load_fit(country)
  # Create a sequence representing time points/waves
  time <- c(1:nrow(contact_data(country)[[2]][,-1]))
  comix_matrix <- as.matrix(contact_data(country)[[2]][,-1])
  num_rows <- nrow(comix_matrix)
  num_cols <- ncol(comix_matrix)
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
    y_pred <- cbind(
      as.data.frame(summary(loaded_fit, pars = param_names[, j], 
                            probs = c(0.05, 0.5, 0.95))$summary),time, unlist(comix_matrix[, j]))
    colnames(y_pred) <- make.names(colnames(y_pred)) # Remove % in the column names
    
    ggplot(y_pred, aes(x = time)) +
      geom_ribbon(aes(ymin = X5., ymax = X95., fill = "95% CI"), alpha = 0.2) +
      geom_line(aes(x = time, y = X50., color = "Median"), size = 1) +
      geom_point(aes(y = unlist(comix_matrix[, j]), color = "CoMix"), size = 2, shape = 16, stroke = 0) +
      scale_color_manual(name = '', values = c("Median" = "#33a02c", "CoMix" = 'black')) +
      scale_fill_manual(name = '', values = c("95% CI" = "#33a02c")) +
      labs(x = "Wave", y = y_labels[j]) +
      theme(
        text = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)
      )
  }
 
    y_labels <- c(
      "0-18 to 0-18",
      "0-18 to 18-45",
      "0-18 to 45-64",
      "0-18 to 65+",
      "18-45 to 0-18",
      "18-45 to 18-45",
      "18-45 to 45-64",
      "18-45 to 65+",
      "45-64 to 0-18",
      "45-64 to 18-45",
      "45-64 to 45-64",
      "45-64 to 65+",
      "65+ to 0-18",
      "65+ to 18-45",
      "65+ to 45-64",
      "65+ to 65+")
    
    figure2 <- ggarrange(plotlist = lapply(1:num_cols, create_plot),
                         labels = "AUTO",
                         ncol = 4, nrow = 4,
                         common.legend = TRUE,
                         legend = "bottom")
return(figure2)
}


