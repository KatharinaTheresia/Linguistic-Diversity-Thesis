#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(tidyr)
library(clValid)
library(corrplot)
library(ggcorrplot) 
library(gridExtra)
library(factoextra)
library(dendextend)

# Load Data Frames
# Loading and cleaning individual data frames
df1 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
                                                 "text", "text", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", "text")) %>%
  rename_with(~ tolower(trimws(.))) 
df2 <- read_excel("IDI_old_cleaned.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df3 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 


#### CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data
target_year <- 2010
merged_data <- df1
merged_data <- full_join(merged_data, df2, by = c("isocode", "year"))
merged_data <- full_join(merged_data, df3, by = c("isocode", "year"))
merged_data <- merged_data[merged_data$year == target_year, ]

# Remove all-NA columns and rows
cols_all_na <- colSums(is.na(merged_data)) == nrow(merged_data)
rows_all_na <- rowSums(is.na(merged_data)) == ncol(merged_data)
merged_data <- merged_data[, !cols_all_na]
merged_data <- merged_data[!rows_all_na, ]

# Extract and rename index columns
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)
merged_data <- merged_data[, c("isocode", "year", idx_columns)]
numeric_data <- merged_data[, idx_columns] 
colnames(numeric_data) <- c("EGDI", "IDI", "EPI")

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)


#### Correlation Plots ####
# Heatmap
plot1 <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (pairwise complete obs.) 2010") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

plot2 <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (complete obs.) 2010") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

# Arrange plots
grid.arrange(plot1, plot2, nrow = 2)



# Pairs plot 
# Define a custom panel for correlation coefficient
panel.cor = function(x, y, digits = 2, cex.cor = 2, alpha = 0.05, ...)
{
  par(usr = c(0, 1, 0, 1)) # axes range
  r = cor.test(x, y) # correlation test 
  if(r$p.value < alpha) star = "*" else star = "" # add star if significant 
  txt = paste(round(r$estimate, digits), star) # define text
  text(0.5, 0.5, txt, cex = cex.cor) # add text to box
}

# Generate the pairs plot
pairs(
  ~ EGDI + IDI +EPI, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Correlation Pairs Plot of Indices (2010)" # Add a title
)
#### Clustering - Complete Correlation Matrix (Average) ####
# Check symmetry and positive semi-definiteness
isSymmetric(cor_matrix)
eigenvalues <- eigen(cor_matrix)$values
all(eigenvalues >= 0)
# all true


# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix)

# Plot hierarchical clustering
hc_avg <- hclust(dist_matrix, method = "average")
plot(hc_avg, main = "Hier. Clustering (complete obs. - average) 2010")


#### Validation Measures: Complete Matrix (Average) ####
# WSS (Within Sum of Squares)
wss_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "wss",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
print(wss_result)

# Display value for optimal k

# optimal = 2k 
print(wss_result$data$y[2])
# 0.009796688


# Silhouette score
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
print(silhouette_result)

# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.4418567


# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
print(gap_stat_result)


# Display value for optimal k
# optimal = 1k
print(gap_result$data$gap[1])
# 0.159562


# Dunn Index 
# Define the range of k-values
k_values <- 2:3  

# Compute Dunn index for each k
dunn_scores <- sapply(k_values, function(k) {
  clusters <- cutree(hc_avg, k = k)  # Generate clusters for k
  dunn(distance = dist_matrix, clusters = clusters)  # Compute Dunn index
})

# Find the optimal k (highest Dunn index)
optimal_k <- k_values[which.max(dunn_scores)]

# Print results
print(dunn_scores)
print(paste("Optimal k based on Dunn index:", optimal_k))

# Display value for optimal k
# optimal = 2k Dunn index = 3.383506  (actually 3, but 3 is infinite)

# Dunn Index visualization
# Filter out heights that produce only one cluster
valid_h <- h_seq[sapply(h_seq, function(h) length(unique(cutree(hc_avg, h = h))) > 1)]
grid()
# Compute Dunn Index only for valid heights
h_dunn <- sapply(valid_h, function(h) {
  clusters <- cutree(hc_avg, h = h)
  dunn(distance = dist_matrix, clusters)
})

# Plot only valid values

plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")
grid()
#### Visualization with optimal number of ks ####

# cutoff for 2 ks 
k <- 2
h_for_k <- hc_avg$height[length(hc_avg$height) - (k - 1)]
print(h_for_k) 
# 0.05857155

# Plot the dendrogram with cutoff line
plot(hc_avg)
abline(h = 0.05857155, col = 'red')

# Dendrogram with rectangular cluster highlights

par(mar = c(5, 5, 8, 5), xpd = NA)   # Bigger top margin (was 5, now 8)
  
plot(hc_avg, 
       main = "",    # No main here
       xlim = c(-10, length(hc_avg$order) + 10),
       hang = -1,
       yaxt = "n",
       ylab = "Distance (1 - correlation)" )
  
axis(2, at = seq(0, 0.4, 0.05), las = 1)
rect.hclust(hc_avg, k = 2, border = 2:4)
  
#Add  the title higher
title(main = "Hierarchical Clustering Dendrogram (2010)", line = 4, cex.main = 1.5, font.main = 2)


# Dendrogram with colored branches using dendextend
# Dendrogram with colored branches using dendextend
avg_col_dend <- as.dendrogram(hc_avg)
avg_col_dend <- dendextend::color_branches(avg_col_dend, k = 2)
plot(avg_col_dend)


#### Assign Variables to Clusters  ####

cut_avg <- cutree(hc_avg, k = 2)

variables_clusters <- data.frame(
  variable = colnames(cor_matrix),  # Variable names from your correlation matrix
  cluster = cut_avg                # Cluster assignments from `cutree()`
)

# Display the number of variables per cluster
count(variables_clusters, cluster)

#### Clustering - Complete Correlation Matrix (Complete) ####
# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix)
hc_cpl <- hclust(dist_matrix, method = "complete")

# Plot Hierarchical Clustering

plot(hc_cpl, main = "Hier. Clustering (complete obs. - complete 2008)")

# almost identical to average linkage


#### Clustering - Pairwise Complete Correlation Matrix (Average) ####
#checking if the pairwise matrix is symmetric and positive semi-definite
isSymmetric(cor_matrix_pair)
eigenvalues <- eigen(cor_matrix_pair)$values
all(eigenvalues >= 0)
#all true

# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix_pair)
hc_avg_pair <- hclust(dist_matrix, method = "average")

# Plot hierarchical clustering

plot(hc_avg_pair, main = "Hier. Clustering (pairwise obs. - average) 2010")


#### Validation Measures: Pairwise Complete Matrix (Average) ####
# WSS (Within Sum of Squares)
wss_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "wss",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print result
print(wss_result)

# Display value for optimal k
# optimal = 2k 
print(wss_result$data$y[2])
#  0.007175599



# Silhouette Score
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print result
print(silhouette_result)


# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.4832649


# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print result
print(gap_stat_result)

# Display value for optimal k
#optimal = 3k
print(gap_result$data$gap[3])
# 0.1999941

# Dunn Index 
# Define the range of k-values
k_values <- 2:3

# Compute Dunn index for each k
dunn_scores <- sapply(k_values, function(k) {
  clusters <- cutree(hc_avg_pair, k = k)  # Generate clusters for k
  dunn(distance = dist_matrix, clusters = clusters)  # Compute Dunn index
})

# Find the optimal k (highest Dunn index)
optimal_k <- k_values[which.max(dunn_scores)]

# Print result
print(dunn_scores)
print(paste("Optimal k based on Dunn index:", optimal_k))

# Display value for optimal k
# optimal = 3k Dunn index = 3.832397

# Dunn Index visualization
# Filter out heights that produce only one cluster
valid_h <- h_seq[sapply(h_seq, function(h) length(unique(cutree(hc_avg, h = h))) > 1)]

# Compute Dunn Index only for valid heights
h_dunn <- sapply(valid_h, function(h) {
  clusters <- cutree(hc_avg_pair, h = h)
  dunn(distance = dist_matrix, clusters)
})

# Plot only valid values

plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")

#### Visualization with optimal number of ks ####

# Cutoff height for 2 clusters 
k <- 2
h_for_k <- hc_avg_pair$height[length(hc_avg_pair$height) - (k - 1)]
print(h_for_k) 
# 0.05857155

# Plot the dendrogram with cutoff line
plot(hc_avg_pair)
abline(h = 0.05857155, col = 'red')


#### Clustering - Pairwise Complete Correlation Matrix (Complete) ####
# Compute distance matrix and perform hierarchical clustering
hc_cpl_pair <- hclust(dist_matrix, method = "complete")

# Plot Hierachical Clustering

plot(hc_cpl_pair, main = "Hier. Clustering (pairwise obs. - complete 2010)")

# almost identical to average linkage again

#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data
target_year <- 2010
merged_data <- df1
merged_data <- inner_join(merged_data, df2, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df3, by = c("isocode", "year"))
merged_data <- merged_data[merged_data$year == target_year, ]

# Extract and rename index columns
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)
merged_data <- merged_data[, c("isocode", "year", idx_columns)]
numeric_data <- merged_data[, idx_columns]
colnames(numeric_data) <- c("EGDI", "IDI", "EPI")

# Check for NAs
any(is.na(numeric_data))
# no NAS > no difference between complete and pairwise obs


# Compute correlation matrix 
cor_matrix <- cor(numeric_data, use = "complete.obs")

# print correlation matrix
print(cor_matrix)
# identical to full join

#### "BROADNESS" OF INDICES ####

# EGDI

# Filter the data frame for the year 2010
df1_filtered <- df1 %>%
  filter(year == 2010) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df1_filtered %>%
  select(-isocode, -year)

# Remove All-NA Columns and Rows
cols_all_na <- colSums(is.na(idc_cols)) == nrow(idc_cols)
rows_all_na <- rowSums(is.na(idc_cols)) == ncol(idc_cols)
idc_cols <- idc_cols[, !cols_all_na]
idc_cols <- idc_cols[!rows_all_na, ]

# compute the correlation matrix
cor_matrix <- cor(idc_cols, use = "pairwise.complete.obs")

# Print the Correlation matrix
print(cor_matrix)

# Summary 
cor_summary <- list(
  mean = mean(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  median = median(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  sd = sd(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  min = min(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  max = max(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE)
)

print(cor_summary)

#$mean = 0.6313618

#$median = 0.6107891

#$sd = 0.1264686

#$min = 0.3708848

#$max = 0.8833078


# IDI_OLD

# Filter the data frame for the year 2010
df2_filtered <- df2 %>%
  filter(year == 2010) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df2_filtered %>%
  select(-isocode, -year)

# Remove All-NA Columns and Rows
cols_all_na <- colSums(is.na(idc_cols)) == nrow(idc_cols)
rows_all_na <- rowSums(is.na(idc_cols)) == ncol(idc_cols)
idc_cols <- idc_cols[, !cols_all_na]
idc_cols <- idc_cols[!rows_all_na, ]

# Compute the correlation matrix

cor_matrix <- cor(idc_cols, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Summary 
cor_summary <- list(
  mean = mean(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  median = median(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  sd = sd(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  min = min(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE),
  max = max(cor_matrix[upper.tri(cor_matrix, diag = FALSE)], na.rm = TRUE)
)

print(cor_summary)
#$mean = 0.5295144

#$median = 0.624939

#$sd = 0.3239281

#$min = -0.1787668

#$max = 0.9781681

# EPI doesn't have indicator values 