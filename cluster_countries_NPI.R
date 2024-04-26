countries <- c("AT", "BE","BG","CY","CZ", "HR","DE", "DK", "EE", "FI", 
               "FR","GR", "HU", "IE", "IS", "IT", "LI","LT", "LU", "LV", 
               "MT", "NL", "NO",
               "PL", "PT","RO", "SE","SK", "SI", "ES")

dim1_data <- data.frame(matrix(nrow = length(countries), ncol = length(mca_factors(countries[[1]])$`Dim 1`)))
colnames(dim1_data) <- paste0("Dim1_", 1:length(mca_factors(countries[[1]])$`Dim 1`))

# Loop through each country
for (i in seq_along(countries)) {
  # Calculate mca_factors for the current country
  mca_c <- mca_factors(countries[i])
  dim1 <- mca_c$`Dim 1`
  
  # Assign Dim 1 values to the corresponding row of the data frame
  dim1_data[i, ] <- dim1
  
  # Set row names of the data frame to be the country names
  rownames(dim1_data)[i] <- countries[i]
}

# Print the resulting data frame
print(dim1_data)


k <- 7  # You can adjust the number of clusters as needed
kmeans_result <- kmeans(dim1_data, centers = k)

# Get cluster assignments for each row
cluster_assignments <- kmeans_result$cluster

# Append the cluster assignments to the original data frame
df_with_clusters <- cbind(dim1_data, Cluster = cluster_assignments)

# Print the data frame with cluster assignments
print(df_with_clusters)

# Assuming you have performed K-means clustering and stored the cluster assignments in 'cluster_assignments'

# Perform PCA
pca_result <- prcomp(dim1_data, scale. = TRUE)

# Extract the first two principal components
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$Row <- rownames(dim1_data)
# Add cluster assignments to the PCA data
pca_data$Cluster <- factor(cluster_assignments)

# Plot the PCA results colored by cluster assignment
library(ggplot2)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster, label = Row)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1) +  # Adjust text position if needed
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "Cluster")
####################
# Elbow Method
wcss <- numeric(length = 10)

for (i in 1:10) {
  kmeans_model <- kmeans(dim1_data, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plotting the Elbow Method
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares", main = "Elbow Method")


######################################################################################

# Perform hierarchical clustering
hc <- hclust(dist(dim1_data))  # Assuming 'df' is your data frame

# Visualize the dendrogram with labeled leaves
plot(hc, main = "Dendrogram of Hierarchical Clustering based on 1st dimension", labels = rownames(dim1_data), cex = 0.8, las = 2)


# Perform hierarchical clustering
hc <- hclust(dist(dim1_data))

# Define a function to rotate labels
rotate_labels <- function(labels) {
  text(0, 0, labels, cex = 0.8, srt = 90, adj = c(1, 0.5), xpd = TRUE)
}

# Plot the dendrogram with rotated labels
plot(hc, main = "Dendrogram of Hierarchical Clustering based on 1st dimension", labels = FALSE)
rotate_labels(rownames(dim1_data))
