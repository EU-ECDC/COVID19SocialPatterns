#' Match Non-CoMix Countries to Similar CoMix Countries Based on NPI Patterns
#'
#' This function matches countries without CoMix contact survey data to countries with
#' CoMix data based on the similarity of their non-pharmaceutical intervention (NPI) patterns.
#' This allows for approximating contact patterns in countries where direct survey data
#' is unavailable.
#'
#' @param comix_countries Character vector. Countries with CoMix contact survey data.
#' @param non_comix_countries Character vector. Countries without CoMix data that need matching.
#' @param top_n Integer. Number of top matches to return for each non-CoMix country. Default is 3.
#' @param cluster_results List or NULL. Optional clustering results from cluster_countries().
#'                        If provided, includes cluster information in the matches.
#' @param time_window Character vector or NULL. Optional date range (c("YYYY-MM-DD", "YYYY-MM-DD"))
#'                    to restrict the comparison period.
#'
#' @return A list containing:
#'   \item{matches}{List of top matches for each non-CoMix country with similarity scores}
#'   \item{all_distances}{Distance matrix between all countries (both CoMix and non-CoMix)}
#'   \item{comix_distances}{Distance matrix between only CoMix countries}
#'   \item{max_comix_dist}{Maximum distance observed between CoMix countries (for normalization)}
#'   \item{common_dates}{Vector of dates used in the comparison}
match_countries <- function(comix_countries, non_comix_countries, top_n = 3, 
                            cluster_results = NULL, time_window = NULL) {
  # Collect MCA factors for all countries
  comix_mca_results <- list()
  for (country in comix_countries) {
    tryCatch({
      comix_mca_results[[country]] <- mca_factors(country)
    }, error = function(e) {
      warning(paste("Error processing", country, ":", e$message))
    })
  }
  
  non_comix_mca_results <- list()
  for (country in non_comix_countries) {
    tryCatch({
      non_comix_mca_results[[country]] <- mca_factors(country)
    }, error = function(e) {
      warning(paste("Error processing", country, ":", e$message))
    })
  }
  
  # Find common dates across all countries
  all_mca_results <- c(comix_mca_results, non_comix_mca_results)
  common_dates <- find_common_dates(all_mca_results)
  
  # If time window is specified, filter dates
  if (!is.null(time_window)) {
    if (length(time_window) == 2) {
      start_date <- as.Date(time_window[1])
      end_date <- as.Date(time_window[2])
      common_dates <- common_dates[common_dates >= start_date & common_dates <= end_date]
      if (length(common_dates) == 0) {
        stop("No common dates found within the specified time window")
      }
    } else {
      warning("Time window should be a vector of two dates. Ignoring time window.")
    }
  }
  
  # Align MCA factors for all countries
  comix_aligned <- align_mca_factors(comix_mca_results, common_dates)
  non_comix_aligned <- align_mca_factors(non_comix_mca_results, common_dates)
  
  # Create a distance matrix for all countries (both CoMix and non-CoMix)
  # This matrix will store the pairwise distances between all countries
  all_distances <- matrix(0, 
                          nrow = length(comix_aligned) + length(non_comix_aligned),
                          ncol = length(comix_aligned) + length(non_comix_aligned))
  
  all_countries <- c(names(comix_aligned), names(non_comix_aligned))
  rownames(all_distances) <- colnames(all_distances) <- all_countries
  # Calculate pairwise distances between all countries using Frobenius norm
  for (i in 1:(length(all_countries)-1)) {
    for (j in (i+1):length(all_countries)) {
      country_i <- all_countries[i]
      country_j <- all_countries[j]
      
      matrix_i <- if (country_i %in% names(comix_aligned)) comix_aligned[[country_i]] else non_comix_aligned[[country_i]]
      matrix_j <- if (country_j %in% names(comix_aligned)) comix_aligned[[country_j]] else non_comix_aligned[[country_j]]
      
      diff_matrix <- matrix_i - matrix_j
      dist <- sqrt(sum(diff_matrix^2))
      
      all_distances[i,j] <- dist
      all_distances[j,i] <- dist
    }
  }
  
  # Extract the CoMix-only distance matrix for reference
  comix_distances <- all_distances[names(comix_aligned), names(comix_aligned)]
  
  # Find maximum distance within CoMix countries to use for normalization
  max_comix_dist <- max(comix_distances[lower.tri(comix_distances)])
  
  # Match non-CoMix countries to closest CoMix countries
  matches <- list()
  for (non_comix_country in names(non_comix_aligned)) {
    # Get distances to all CoMix countries
    distances <- all_distances[non_comix_country, names(comix_aligned)]
    
    # Sort distances and get top N matches
    sorted_indices <- order(distances)
    top_matches <- list()
    
    for (i in 1:min(top_n, length(distances))) {
      comix_country <- names(distances)[sorted_indices[i]]
      distance <- distances[sorted_indices[i]]
      
      # Calculate similarity score (100% = identical, 0% = maximally different)
      similarity_score <- max(0, 100 * (1 - distance / max_comix_dist))
      
      # Get cluster information if available
      cluster_info <- NULL
      if (!is.null(cluster_results) && 
          !is.null(cluster_results$clustering_results$ward.D2$cluster_assignments)) {
        
        # Try to get cluster assignments
        cluster_assignments <- cluster_results$clustering_results$ward.D2$cluster_assignments
        
        # Check if both countries are in the cluster assignments
        if (comix_country %in% names(cluster_assignments) && 
            non_comix_country %in% names(cluster_assignments)) {
          
          comix_cluster <- cluster_assignments[comix_country]
          non_comix_cluster <- cluster_assignments[non_comix_country]
          
          cluster_info <- list(
            comix_cluster = comix_cluster,
            non_comix_cluster = non_comix_cluster,
            same_cluster = (comix_cluster == non_comix_cluster)
          )
        }
      }
      
      top_matches[[i]] <- list(
        matched_to = comix_country,
        distance = distance,
        similarity_score = similarity_score,
        rank = i,
        cluster_info = cluster_info
      )
    }
    
    matches[[non_comix_country]] <- top_matches
  }
  
  return(list(
    matches = matches,
    all_distances = all_distances,
    comix_distances = comix_distances,
    max_comix_dist = max_comix_dist,
    common_dates = common_dates
  ))
}

#' Visualize Country Matching Results
#'
#' This function creates visualizations of country matching results, showing which
#' CoMix countries are most similar to each non-CoMix country in terms of NPI patterns.
#'
#' @param match_results List. The output from the match_countries function.
#' @param plot_type Character. Type of visualization to create: "heatmap", "network", or "both".
#'                  Default is c("heatmap", "network", "both"), which will use the first value.
#' @param show_top_n Integer. Number of top matches to display for each non-CoMix country.
#'                   Default is 1.
#'
#' @return A data frame with match information, including country pairs and similarity scores.
#'         The function also produces visualizations based on the specified plot_type.
visualize_matches <- function(match_results, plot_type = c("heatmap", "network", "both"), 
                              show_top_n = 1) {
  plot_type <- match.arg(plot_type)
  matches <- match_results$matches
  
  # Create a data frame of top matches
  match_df <- do.call(rbind, lapply(names(matches), function(country) {
    # Get the top N matches for this country
    top_matches <- matches[[country]][1:min(show_top_n, length(matches[[country]]))]
    
    do.call(rbind, lapply(top_matches, function(match) {
      cluster_same <- if (!is.null(match$cluster_info)) {
        match$cluster_info$same_cluster
      } else {
        NA
      }
      
      data.frame(
        non_comix_country = country,
        matched_to = match$matched_to,
        distance = match$distance,
        similarity = match$similarity_score,
        rank = match$rank,
        same_cluster = cluster_same
      )
    }))
  }))
  
  # Print matches
  print(match_df)
  
  # Load required packages
  if (plot_type %in% c("heatmap", "both") && requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    
    # Create heatmap visualization
    p <- ggplot(match_df, aes(x = non_comix_country, y = matched_to)) +
      geom_tile(aes(fill = similarity), color = "white") +
      scale_fill_gradient(low = "red", high = "green", name = "Similarity (%)") +
      geom_text(aes(label = sprintf("%.1f%%", similarity)), size = 3) +
      theme_minimal() +
      labs(title = "Country Matches Based on NPI Similarity",
           subtitle = paste("Top", show_top_n, "matches shown"),
           x = "Non-CoMix Countries", 
           y = "Matched CoMix Countries") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(angle = 0, hjust = 1))
    
    print(p)
  }
  
  # Create network visualization
  if (plot_type %in% c("network", "both") && 
      requireNamespace("igraph", quietly = TRUE) && 
      requireNamespace("ggraph", quietly = TRUE)) {
    
    library(igraph)
    library(ggraph)
    
    edges <- match_df[, c("non_comix_country", "matched_to", "similarity", "distance")]
    names(edges) <- c("from", "to", "similarity", "distance")
    
    comix_countries <- unique(edges$to)
    non_comix_countries <- unique(edges$from)
    nodes <- data.frame(
      id = c(comix_countries, non_comix_countries),
      type = c(rep("CoMix", length(comix_countries)), 
               rep("Non-CoMix", length(non_comix_countries)))
    )
    
    # Create graph
    g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
    
    # Set edge weights based on similarity
    E(g)$width <- E(g)$similarity / 20  # Scale for visibility
    
    # Create network plot
    network_plot <- ggraph(g, layout = "fr") +
      geom_edge_link(aes(width = width, alpha = similarity/100),
                     arrow = arrow(length = unit(2, 'mm')), 
                     end_cap = circle(3, 'mm')) +
      geom_node_point(aes(color = type), size = 5) +
      # This line needs to be changed - "id" isn't a valid column name
      # Original: geom_node_text(aes(label = id), repel = TRUE) 
      # Corrected version:
      geom_node_text(aes(label = name), repel = TRUE) +
      scale_edge_width(range = c(0.5, 2)) +
      scale_edge_alpha(range = c(0.3, 1)) +
      scale_color_manual(values = c("CoMix" = "#0072B2", "Non-CoMix" = "#D55E00")) +
      theme_graph() +
      labs(title = "Network of NPI Similarity Matches",
           subtitle = paste("Top", show_top_n, "matches shown"))
    
    print(network_plot)
  }
  
  return(match_df)
}