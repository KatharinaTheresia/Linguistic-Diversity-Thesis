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
df1 <- read_excel("MCI_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df2 <- read_excel("Digital_STRI_inverted.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df3 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
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
df4 <- read_excel("DAIforweb_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df5 <- read_excel("IDI_old_cleaned.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df6 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 

#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}

####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2014
merged_data <- df1
merged_data <- full_join(merged_data, df2, by = c("isocode", "year"))
merged_data <- full_join(merged_data, df3, by = c("isocode", "year"))
merged_data <- full_join(merged_data, df4, by = c("isocode", "year"))
merged_data <- full_join(merged_data, df5, by = c("isocode", "year"))
merged_data <- full_join(merged_data, df6, by = c("isocode", "year"))
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
colnames(numeric_data) <- c("MCI", "DSTRI", "EGDI", 
                            "DAI", "IDI", "EPI")

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
  ggtitle("Correlation Heatmap (pairwise complete obs.) 2014") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

plot2 <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (complete obs.) 2014") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

# Arrange plots
save_plot("2014_01_heatmaps.jpg", {
  grid.arrange(plot1, plot2, nrow = 2)
})


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
save_plot("2014_02_pairs_idx.jpg", pairs(~ MCI + DSTRI + EGDI + DAI + IDI + EPI, data = numeric_data, lower.panel = panel.cor,
      cex.labels = 1, main = "Correlation Pairs Plot of Indices (2014)"))


#### Clustering - Complete Correlation Matrix (Average) ####
# Check symmetry and positive semi-definiteness
isSymmetric(cor_matrix)
eigenvalues <- eigen(cor_matrix)$values
all(eigenvalues >= 0)
# all true

# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix)
hc_avg <- hclust(dist_matrix, method = "average")

# Plot hierarchical clustering
save_plot("2014_03_clust_avg.jpg", {
  plot(hc_avg, main = "Hier. Clustering (complete obs. - average) 2014")
})

#### Validation Measures: Complete Matrix (Average) ####
# WSS (Within Sum of Squares)
wss_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "wss",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2014_04_wss.jpg", print(wss_result))

# Display value for optimal k
# optimal = 3k 
print(wss_result$data$y[3])
# 0.01857145


# Silhouette Score


# Run fviz_nbclust
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 5,
  verbose = FALSE
)

silhouette_result + labs(
  title = "Hierarchical Clustering - Complete Observations (2014)"
)

# Print the result
save_plot("2014_05_silhouette.jpg", print(silhouette_result))

# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.7653343

# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2014_06_gap_stat.jpg", print(gap_stat_result))

# Display value for optimal k
# optimal = 3k
print(gap_result$data$gap[3])
# 0.1999941


# Dunn Index 
# Define the range of k-values
k_values <- 2:5 

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
# optimal = 3k Dunn index = 2.8942058

# Dunn Index visualization
# Filter out heights that produce only one cluster
valid_h <- h_seq[sapply(h_seq, function(h) length(unique(cutree(hc_avg, h = h))) > 1)]

# Compute Dunn Index only for valid heights
h_dunn <- sapply(valid_h, function(h) {
  clusters <- cutree(hc_avg, h = h)
  dunn(distance = dist_matrix, clusters)
})

# Filter k-values where at least two clusters are formed
valid_k <- k_seq[sapply(k_seq, function(k) length(unique(cutree(hc_avg, k = k))) > 1)]

# Compute Dunn Index only for valid k-values
k_dunn <- sapply(valid_k, function(x) {
  clusters <- cutree(hc_avg, k = x)
  dunn(distance = dist_matrix, clusters)
})

# Plot Dunn Index over height
save_plot("2014_07_dunn.jpg", {
  plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")
  grid()
})
#### Visualization with optimal number of ks ####

# Cutoff height for 3 clusters 
k <- 3
h_for_k <- hc_avg$height[length(hc_avg$height) - (k - 1)]
print(h_for_k) 
# 0.05249192

# Plot the dendrogram with cutoff line
plot(hc_avg)
abline(h = 0.05249192, col = 'red')

# Dendrogram with rectangular cluster highlights
save_plot("2014_08_dend_high.jpg", {
  par(mar = c(5, 5, 5, 5), xpd = NA)  # Allow plotting outside the margins
  
  plot(hc_avg, 
       main = "Hierarchical Clustering Dendrogram (2014)",
       xlim = c(-10, length(hc_avg$order) + 10),  # Larger x-axis range
       hang = -1,                                 # Align leaves at the same baseline
       yaxt = "n",
       ylab = "Distance (1 - correlation)" )      # Suppress default y-axis ticks
  
  axis(2, at = seq(0, 0.4, 0.05), las = 1)         # Add custom y-axis ticks
  rect.hclust(hc_avg, k = 3, border = 2:4)         # Highlight clusters
})

# Dendrogram with colored branches using dendextend
avg_col_dend <- as.dendrogram(hc_avg)
avg_col_dend <- dendextend::color_branches(avg_col_dend, k = 3)
save_plot("2014_09_dend_col.jpg", {plot(avg_col_dend)})



#### Assign Variables to Clusters  ####

cut_avg <- cutree(hc_avg, k = 3)

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
save_plot("2014_10_clust_cpl.jpg", {
  plot(hc_cpl, main = "Hier. Clustering (complete obs. - complete 2014)")
})
#almost identical to average linkage

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
save_plot("2014_11_clust_pair_avg.jpg", {
  plot(hc_avg_pair, main = "Hier. Clustering (pairwise obs. - average) 2014")
})

#### Validation Measures: Pairwise Complete Matrix (Average) ####
# WSS (Within Sum of Squares)
wss_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "wss",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print result
save_plot("2014_12_pair_wss.jpg", print(wss_result))

# Display value for optimal k
# optimal = 3k 
print(wss_result$data$y[3])
# 0.02187806



# Run fviz_nbclust
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 5,
  verbose = FALSE
)

silhouette_result + labs(
  title = "Hierarchical Clustering - Pairwise Complete Observations (2014)"
)


# Print result
save_plot("2014_13_pair_silhouette.jpg", print(silhouette_result))

# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.7682163


# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print result
save_plot("2014_14_pair_gap_stat.jpg", print(gap_stat_result))

# Display value for optimal k
#optimal = 3k
print(gap_result$data$gap[3])
# 0.1999941

# Dunn Index 
# Define the range of k-values
k_values <- 2:5  

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
# optimal = 3k Dunn index = 3.087391 

# Dunn Index visualization
# Filter out heights that produce only one cluster
valid_h <- h_seq[sapply(h_seq, function(h) length(unique(cutree(hc_avg, h = h))) > 1)]

# Compute Dunn Index only for valid heights
h_dunn <- sapply(valid_h, function(h) {
  clusters <- cutree(hc_avg, h = h)
  dunn(distance = dist_matrix, clusters)
})

# Filter k-values where at least two clusters are formed
valid_k <- k_seq[sapply(k_seq, function(k) length(unique(cutree(hc_avg, k = k))) > 1)]

# Compute Dunn Index only for valid k-values
k_dunn <- sapply(valid_k, function(x) {
  clusters <- cutree(hc_avg_pair, k = x)
  dunn(distance = dist_matrix, clusters)
})

# Plot Dunn Index over height
save_plot("2014_15_pair_dunn.jpg", {
  plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")
  grid()
})

#### Visualization with optimal number of ks ####

# Cutoff height for 3 clusters 
k <- 3
h_for_k <- hc_avg_pair$height[length(hc_avg_pair$height) - (k - 1)]
print(h_for_k) 
# 0.05456086

# Plot the dendrogram with cutoff line
plot(hc_avg_pair)
abline(h = 0.05456086, col = 'red')


#### Clustering - Pairwise Complete Correlation Matrix (Complete) ####
# Compute distance matrix and perform hierarchical clustering
hc_cpl_pair <- hclust(dist_matrix, method = "complete")

# Plot Hierachical Clustering
save_plot("2014_16_clust_cpl_pair.jpg", {
  plot(hc_cpl_pair, main = "Hier. Clustering (pairwise obs. - complete 2014)")
})
# almost identical to average linkage again

#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data
target_year <- 2014
merged_data <- df1
merged_data <- inner_join(merged_data, df2, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df3, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df4, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df5, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df6, by = c("isocode", "year"))
merged_data <- merged_data[merged_data$year == target_year, ]

# Extract and rename index columns
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)
merged_data <- merged_data[, c("isocode", "year", idx_columns)]
numeric_data <- merged_data[, idx_columns]
colnames(numeric_data) <- c("MCI", "DSTRI", "EGDI", 
                            "DAI", "IDI", "EPI")

# Check for NAs
any(is.na(numeric_data))
#no NAS > no difference between complete and pairwise obs


# Compute correlation matrix 
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Display correlation matrix
print(cor_matrix)

# Results identical to full join - complete obs.

#####>>>>>>>>>THREE CLUSTERS SEEM TO BE THE OPTIMAL NUMBER OF CLUSTERS FOR 2014"


#### "BROADNESS" OF INDICES ####

# MCI 

# Filter the data frame for the year 2014
df1_filtered <- df1 %>%
  filter(year == 2014) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df1_filtered %>%
  select(-isocode, -year)

# Remove all-NA columns and rows
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

#$mean = 0.5068802

#$median =  0.5494316

#$sd = 0.2286834

#$min = -0.1901187

#$max = 0.9783431


# DSTRI

# Filter the data frame for the year 2014
df2_filtered <- df2 %>%
  filter(year == 2014) %>%
  select(isocode, year, matches(".*\\(sub-idx\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
subidx_cols <- df2_filtered %>%
  select(-isocode, -year)

# Remove All-NA Columns and Rows
cols_all_na <- colSums(is.na(subidx_cols)) == nrow(subidx_cols)
rows_all_na <- rowSums(is.na(subidx_cols)) == ncol(subidx_cols)
subidx_cols <- subidx_cols[, !cols_all_na]
subidx_cols <- subidx_cols[!rows_all_na, ]

# compute the correlation matrix
cor_matrix <- cor(subidx_cols, use = "pairwise.complete.obs")

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

#$mean = 0.2649092

#$median = 0.2585608

#$sd = 0.1017736

#$min = 0.05019454

#$max = 0.4063281


# EGDI 

# Filter the data frame for the year 2014
df3_filtered <- df3 %>%
  filter(year == 2014) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df3_filtered %>%
  select(-isocode, -year)

# Remove all-NA columns and rows
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
#$mean = 0.7124418

#$median =  0.735468

#$sd = 0.2997052

#$min = 0.1381489

#$max = 0.9371392

# IDI_Old 

# Filter the data frame for the year 2014
df5_filtered <- df5 %>%
  filter(year == 2014) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df5_filtered %>%
  select(-isocode, -year)

# Remove all-NA columns and rows
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
#$mean = 0.5361864

#$median =  0.6411502

#$sd = 0.2997052

#$min = -0.05289802

#$max = 0.9783431


