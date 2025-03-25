#' Calculate and Plot Silhouette Scores to Find Optimal Number of Clusters
#'
#' This function determines the optimal number of clusters for hierarchical clustering
#' by calculating silhouette scores for a range of possible cluster numbers.
#'
#' @param distance_matrix Numeric matrix. A symmetric distance matrix with countries as row/column names.
#' @param max_k Integer. The maximum number of clusters to consider. Default is 10.
#'
#' @return Integer. The optimal number of clusters based on the highest silhouette score.
plot_silhouette <- function(distance_matrix, max_k = 10) {
  # We need at least k+1 objects to form k clusters
  n_objects <- nrow(distance_matrix)
  if (max_k >= n_objects) {
    max_k <- n_objects - 1
    warning(paste("Reduced max_k to", max_k, "since we only have", n_objects, "objects"))
  }
  
  # We need at least 2 clusters to compute silhouette
  if (max_k < 2) {
    stop("Not enough objects for meaningful clustering")}
  
  # Calculate silhouette scores for each number of clusters from 2 to max_k
  silhouette_scores <- numeric(max_k - 1)
  valid_k <- numeric(max_k - 1)
  count <- 0
  for (k in 2:max_k) {
    tryCatch({
      # Perform hierarchical clustering with Ward's method
      hc <- hclust(as.dist(distance_matrix), method = "ward.D2")
      clusters <- cutree(hc, k = k)
      
      # Check if any cluster has only one object (singleton)
      cluster_sizes <- table(clusters)
      if (any(cluster_sizes == 1)) {
        warning(paste("Skipping k =", k, "because it creates singleton clusters"))
        silhouette_scores[k-1] <- NA
      } else {
        # Compute silhouette width when all clusters have at least 2 members
        sil <- silhouette(clusters, dist = as.dist(distance_matrix))
        silhouette_scores[k-1] <- mean(sil[, "sil_width"])
        count <- count + 1
        valid_k[count] <- k
      }
    }, error = function(e) {
      warning(paste("Error computing silhouette for k =", k, ":", e$message))
      silhouette_scores[k-1] <- NA
    })
  }
  
  # Remove NAs
  valid_indices <- !is.na(silhouette_scores)
  valid_scores <- silhouette_scores[valid_indices]
  valid_k_values <- 2:max_k
  valid_k_values <- valid_k_values[valid_indices]
  
  if (length(valid_scores) == 0) {
    warning("Could not calculate silhouette scores for any k value")
    return(8)  # Return default k=8 as a fallback
  }
  
  # # Plot silhouette scores to visualize the optimal k
  plot(valid_k_values, valid_scores, 
       type = "b", 
       xlab = "Number of Clusters", 
       ylab = "Average Silhouette Width",
       main = "Optimal Number of Clusters")
  
  # Return the optimal number of clusters
  optimal_k <- valid_k_values[which.max(valid_scores)]
  
  points(optimal_k, max(valid_scores), col = "red", pch = 19, cex = 2)
  text(optimal_k, max(valid_scores), labels = paste("k =", optimal_k), pos = 3, col = "red")
  
  return(optimal_k)
}

#' Find Common Date Range Across Multiple Countries
#'
#' This function identifies a common date range that exists across all provided MCA result objects.
#'
#' @param mca_results A list of data frames, each containing date information in the first column.
#'        Each data frame represents MCA factor data for a different country.
#'
#' @return A vector of dates representing the common date range across all countries,
#'         as a daily sequence from the latest start date to the earliest end date.
find_common_dates <- function(mca_results) {
  # Extract dates from each country
  all_dates <- lapply(mca_results, function(x) x[, 1])
  
  # Find the latest start date and earliest end date
  max_start <- do.call(max, lapply(all_dates, min))
  min_end <- do.call(min, lapply(all_dates, max))
  
  # Create sequence of common dates
  common_dates <- seq.Date(from = max_start, to = min_end, by = "day")
  
  return(common_dates)
}

#' Align MCA Factor Matrices by Common Dates
#'
#' This function aligns the MCA factor matrices from different countries to a common
#' set of dates. It filters each country's data to include only the dates within the 
#' common date range and returns them as matrices suitable for distance calculations.
#'
#' @param mca_results A list of data frames, each containing date information in the first column
#'        and MCA factor values in the remaining columns.
#' @param common_dates A vector of dates representing the common date range to use for alignment.
#'
#' @return A list of matrices, one per country, containing the MCA factor values for the
#'         common date range. Each matrix has the same dimensions (number of common dates × 
#'         number of MCA factors) and corresponds to the same sequence of dates.
align_mca_factors <- function(mca_results, common_dates) {
  aligned_factors <- list()
  
  for (country in names(mca_results)) {
    country_data <- mca_results[[country]]
    colnames(country_data)[1] <- "date"  # Ensure date column is named consistently
    
    # Filter to common date range
    country_aligned <- country_data %>%
      filter(date %in% common_dates) %>%
      arrange(date) #%>%
    #   select(-date)  # Remove date column for distance calculations
    
    country_aligned <- country_aligned[, -1]
    
    aligned_factors[[country]] <- as.matrix(country_aligned)
  }
  
  return(aligned_factors)
}

#' Perform Hierarchical Clustering of Countries Using Multiple Methods
#'
#' This function clusters countries based on their MCA factors using hierarchical clustering
#' with one or more linkage methods. It can automatically determine the optimal number of
#' clusters using silhouette analysis.
#'
#' @param countries Character vector. List of country codes to include in the clustering.
#' @param methods Character vector. Clustering methods to use (e.g., "ward.D2", "single", "complete").
#'                Default is c("ward.D2", "single", "complete").
#' @param k Integer or NULL. The number of clusters to create. If NULL (default), the optimal
#'          number of clusters is determined automatically.
#' @param max_k Integer. The maximum number of clusters to consider when determining the optimal k.
#'              Default is 15.
#'
#' @return A list containing:
#'   \item{clustering_results}{List of results for each clustering method}
#'   \item{distance_matrix}{Distance matrix used for clustering}
#'   \item{aligned_factors}{Aligned MCA factors for each country}
#'   \item{common_dates}{Vector of dates common to all countries}
#'   \item{optimal_k}{The number of clusters used}
cluster_countries <- function(countries, methods = c("ward.D2", "single", "complete"), k = NULL, max_k = 15) {
  # Collect MCA factors for all countries
  mca_results <- list()
  for (country in countries) {
    tryCatch({
      mca_results[[country]] <- mca_factors(country)
    }, error = function(e) {
      warning(paste("Error processing", country, ":", e$message))
    })
  }
  
  # Find common dates
  common_dates <- find_common_dates(mca_results)
  # Align MCA factors
  aligned_factors <- align_mca_factors(mca_results, common_dates)
  # Compute distance matrix between countries using Frobenius norm
  distance_matrix <- matrix(0, nrow = length(aligned_factors), 
                            ncol = length(aligned_factors),
                            dimnames = list(names(aligned_factors), 
                                            names(aligned_factors)))
  # Calculate pairwise distances between countries
  for (i in 1:(length(aligned_factors)-1)) {
    for (j in (i+1):length(aligned_factors)) {
      # Compute Frobenius norm between matrices
      diff_matrix <- aligned_factors[[i]] - aligned_factors[[j]]
      dist <- sqrt(sum(diff_matrix^2))
      
      distance_matrix[i,j] <- dist
      distance_matrix[j,i] <- dist
    }
  }
  
  # Convert to dist object for use with hclust
  dist_obj <- as.dist(distance_matrix)
  
  # If k is not specified, determine the optimal number of clusters
  if (is.null(k)) {
    cat("Determining optimal number of clusters...\n")
    tryCatch({
      optimal_k <- plot_silhouette(distance_matrix, max_k = max_k)
      cat("Optimal number of clusters:", optimal_k, "\n")
      k <- optimal_k
    }, error = function(e) {
      warning(paste("Error determining optimal k:", e$message, "\nUsing default k = 8"))
      k <- 8
    })
  }
  
  clustering_results <- list()
  
  # Set up plotting layout based on number of methods
  if (length(methods) > 1) {
    par(mfrow = c(1, length(methods)))
  }
  
  # Perform hierarchical clustering for each method
  for (method in methods) {
    hc <- hclust(dist_obj, method = method)
    # Plot dendrogram
    plot(hc, 
         main = paste("Hierarchical Clustering -", method), 
         xlab = "Countries", 
         sub = paste("Distance method:", method, "| k =", k),
         hang = -1)
    
    # Add cluster rectangles
    rect.hclust(hc, k = k, border = 2:6)
    # Cut the tree to get cluster assignments for each country
    clusters <- cutree(hc, k = k)
    
    # Store results
    clustering_results[[method]] <- list(
      hierarchical_clustering = hc,
      cluster_assignments = clusters
    )
  }
  
  # Reset plot layout
  if (length(methods) > 1) {
    par(mfrow = c(1, 1))
  }
  
  # Return clustering results and distance matrix
  return(list(
    clustering_results = clustering_results,
    distance_matrix = distance_matrix,
    aligned_factors = aligned_factors,
    common_dates = common_dates,
    optimal_k = k
  ))
}


#' Compare Cluster Assignments Across Different Clustering Methods
#'
#' This function analyzes how different hierarchical clustering methods group the
#' same set of countries, providing both tabular and visual comparisons.
#'
#' @param clustering_result List. The output from the cluster_countries function.
#' @param k Integer or NULL. The number of clusters to use for comparison. If NULL (default),
#'          uses the optimal_k from the clustering_result.
#'
#' @return A list containing:
#'   \item{comparison_table}{Data frame with cluster assignments for each country by method}
#'   \item{agreement_matrix}{Matrix of agreement scores between methods}
compare_clustering_methods <- function(clustering_result, k = NULL) {
  # If k is not provided, use the optimal k from clustering_result
  if (is.null(k)) {
    k <- clustering_result$optimal_k
    cat("Using optimal k =", k, "from clustering result\n")
  }
  
  methods <- names(clustering_result$clustering_results)
  
  # Get cluster assignments for each method
  cluster_assignments <- lapply(methods, function(method) {
    # If cluster assignments already exist with the requested k, use them
    if (!is.null(clustering_result$clustering_results[[method]]$cluster_assignments)) {
      assignments <- clustering_result$clustering_results[[method]]$cluster_assignments
      # Check if the assignments were made using the requested k
      if (length(unique(assignments)) == k) {
        return(assignments)
      }
    }
    
    # Otherwise calculate new assignments with the requested k
    hc <- clustering_result$clustering_results[[method]]$hierarchical_clustering
    return(cutree(hc, k = k))
  })
  
  names(cluster_assignments) <- methods
  
  # Create a data frame with cluster assignments for each method
  # This shows how each country is classified by each method
  comparison_df <- as.data.frame(cluster_assignments)
  comparison_df$Country <- rownames(comparison_df)
  comparison_df <- comparison_df[, c("Country", methods)]
  
  # Calculate agreement between methods
  agreement_matrix <- matrix(0, nrow = length(methods), ncol = length(methods),
                             dimnames = list(methods, methods))
  # Calculate pairwise agreement between all methods
  for (i in 1:length(methods)) {
    for (j in 1:length(methods)) {
      # Use adjusted Rand index to measure agreement
      if (requireNamespace("fossil", quietly = TRUE)) {
        agreement_matrix[i, j] <- fossil::adj.rand.index(
          cluster_assignments[[i]], 
          cluster_assignments[[j]]
        )
      } else {
        # Simple percentage agreement if fossil package not available
        agreement_matrix[i, j] <- sum(cluster_assignments[[i]] == cluster_assignments[[j]]) / 
          length(cluster_assignments[[i]])
      }
    }
  }
  
  # Visualize the agreement matrix
  if (requireNamespace("pheatmap", quietly = TRUE)) {
    pheatmap::pheatmap(
      agreement_matrix,
      main = paste("Agreement Between Clustering Methods (k =", k, ")"),
      display_numbers = TRUE,
      number_format = "%.2f"
    )
  } else {
    cat("Install pheatmap package for better visualization of the agreement matrix\n")
    print(agreement_matrix)
  }
  
  # Return the comparison data frame and agreement matrix
  return(list(
    comparison_table = comparison_df,
    agreement_matrix = agreement_matrix
  ))
}