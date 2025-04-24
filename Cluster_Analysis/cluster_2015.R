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
df3 <- read_excel("IDI_old_cleaned.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))

#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}

####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2015
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
colnames(numeric_data) <- c("MCI", "DSTRI", "IDI")

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

# very minimal difference ( 0.9513504 vs. 0.9453847 for MCI ~ IDI)


#### Correlation Plots ####
# Heatmap
plot <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                   type = "lower",  
                   colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap 2015") +
  
  # Customize the plot's appearance
  theme(
    axis.text.x = element_text(size = 8),   
    axis.text.y = element_text(size = 8),   
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  
  )

# Display the heatmap
save_plot("2015_01_heatmap.jpg", print(plot))

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
save_plot("2015_02_pairs_idx.jpg", pairs(~ MCI + DSTRI + IDI, data = numeric_data, lower.panel = panel.cor,
      cex.labels = 1, main = "Correlation Pairs Plot of Indices (2015)"))

#### Clustering - Complete Correlation Matrix (Average) ####
# Check symmetry and positive semi-definiteness
isSymmetric(cor_matrix)
eigenvalues <- eigen(cor_matrix)$values
all(eigenvalues >= 0)
#all true

# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix)
hc_avg <- hclust(dist_matrix, method = "average")

# Plot hierarchical clustering
save_plot("2015_03_clust_avg.jpg", {
  plot(hc_avg, main = "Hier. Clustering (complete obs. - average) 2015")
})

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
save_plot("2015_04_wss.jpg", print(wss_result))

# Display value for optimal k
# optimal = 2k 
print(wss_result$data$y[2])
# 0.002444483


# Silhouette Score
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2015_05_silhouette.jpg", print(silhouette_result))

# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.6061531

# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 2,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2015_06_gap_stat.jpg", print(gap_stat_result))

# Display value for optimal k
# optimal = 2k
print(gap_result$data$gap[2])
# 0 0.1753113


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
# optimal = 2k (3 actually, but 3 is infinite) Dunn index = 9.334198

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
save_plot("2015_07_dunn.jpg", {
  plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")
  grid()
})

#### Visualization with optimal number of ks ####

# Cutoff height for 2 clusters 
k <- 2
h_for_k <- hc_avg$height[length(hc_avg$height) - (k - 1)]
print(h_for_k) 
# 0.04864958

# Plot the dendrogram with cutoff line
plot(hc_avg)
abline(h = 0.04864958, col = 'red')


# Dendrogram with rectangular cluster highlights
save_plot("2015_08_dend_high.jpg", {
  par(mar = c(5, 5, 5, 5), xpd = NA)  # Allow plotting outside the margins
  
  plot(hc_avg, 
       main = "Hierarchical Clustering Dendrogram (2015)",
       xlim = c(-10, length(hc_avg$order) + 10),  # Larger x-axis range
       hang = -1,                                 # Align leaves at the same baseline
       yaxt = "n",
       ylab = "Distance (1 - correlation)" )      # Suppress default y-axis ticks
  
  axis(2, at = seq(0, 0.4, 0.05), las = 1)         # Add custom y-axis ticks
  rect.hclust(hc_avg, k = 2, border = 2:4)         # Highlight clusters
})

# Dendrogram with colored branches using dendextend
# Convert the hclust object into a dendrogram
avg_col_dend <- as.dendrogram(hc_avg)
avg_col_dend <- dendextend::color_branches(avg_col_dend, k = 2)
save_plot("2015_09_dend_col.jpg", {plot(avg_col_dend)})

# Plot the colored dendrogram
plot(
  avg_col_dend,
  main = "Hierarchical Clustering Dendrogram (2015)",  
  ylab = "Distance (1 - correlation)",                 
  ylim = c(0, 0.4)
)


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
save_plot("2015_10_clust_cpl.jpg", {
  plot(hc_cpl, main = "Hier. Clustering (complete obs. - complete 2015)")
})

#almost identical to average linkage

#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data
target_year <- 2015
merged_data <- df1
merged_data <- inner_join(merged_data, df2, by = c("isocode", "year"))
merged_data <- inner_join(merged_data, df3, by = c("isocode", "year"))
merged_data <- merged_data[merged_data$year == target_year, ]

# Extract and rename index columns
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)
merged_data <- merged_data[, c("isocode", "year", idx_columns)]
numeric_data <- merged_data[, idx_columns]
colnames(numeric_data) <- c("MCI", "DSTRI", "IDI")

# Check for NAs
any(is.na(numeric_data))
#no NAS > no difference between complete and pairwise obs

# Compute correlation matrix

cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Display correlation matrices
print(cor_matrix)

#Results are almost identical to full join 

#TWO CLUSTERS SEEM TO MAKE MOST SENSE

#### "BROADNESS" OF INDICES ####

# MCI

# Filter the data frame for the year 2015
df1_filtered <- df1 %>%
  filter(year == 2015) %>%
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

#$mean = 0.5176984

#$median = 0.5772469

#$sd = 0.2338152

#$min = -0.2162859

#$max =0.9805164

# DSTRI

# Filter the data frame for the year 2015
df2_filtered <- df2 %>%
  filter(year == 2015) %>%
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

#$mean = 0.2647418

#$median = 0.2627555

#$sd = 0.09578748

#$min = 0.05469642

#$max = 0.4032143

# IDI_Old

# Filter the data frame for the year 2015
df3_filtered <- df3 %>%
  filter(year == 2015) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df3_filtered %>%
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

#$mean = 0.5111999

#$median = 0.5777252

#$sd = 0.3010424

#$min = -0.1031766

#$max = 0.963146

