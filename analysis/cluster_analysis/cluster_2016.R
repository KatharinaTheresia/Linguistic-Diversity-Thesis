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
target_year <- 2016
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

# very minimal difference in results 

#MCI      DSTRI      EGDI       DAI       IDI       EPI
#MCI  1.0000000 0.5082645 0.9522570 0.9348005 0.9527395 0.8033203
#DSTRI 0.5082645 1.0000000 0.5191502 0.5049830 0.5305610 0.4282446
#EGDI 0.9522570 0.5191502 1.0000000 0.9497143 0.9647489 0.8912714
#DAI  0.9348005 0.5049830 0.9497143 1.0000000 0.9520882 0.8181081
#IDI  0.9527395 0.5305610 0.9647489 0.9520882 1.0000000 0.7796064
#EPI  0.8033203 0.4282446 0.8912714 0.8181081 0.7796064 1.0000000

#MCI      DSTRI      EGDI       DAI       IDI       EPI
#MCI  1.0000000 0.5132768 0.9524146 0.9445237 0.9458253 0.8253688
#DSTRI 0.5132768 1.0000000 0.5238315 0.5100199 0.5305610 0.4346211
#EGDI 0.9524146 0.5238315 1.0000000 0.9534228 0.9504293 0.8734912
#DAI  0.9445237 0.5100199 0.9534228 1.0000000 0.9428260 0.8419258
#IDI  0.9458253 0.5305610 0.9504293 0.9428260 1.0000000 0.7405303
#EPI  0.8253688 0.4346211 0.8734912 0.8419258 0.7405303 1.0000000

#### Correlation Plots ####
# Heatmap
plot <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                   type = "lower",  
                   colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap 2016") +
  
  # Customize the plot's appearance
  theme(
    axis.text.x = element_text(size = 8),   
    axis.text.y = element_text(size = 8),   
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  
  )

# Display the heatmap
save_plot("2016_01_heatmap.jpg", print(plot))

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
save_plot("2016_02_pairs_idx.jpg", pairs(~ MCI + DSTRI + EGDI + DAI + IDI + EPI, data = numeric_data, lower.panel = panel.cor,
      cex.labels = 1, main = "Correlation Pairs Plot of Indices (2016)"))

#### Clustering - Complete Correlation Matrix (Average) ####
# Check symmetry and positive semi-definiteness
isSymmetric(cor_matrix)
eigenvalues <- eigen(cor_matrix)$values
all(eigenvalues >= 0)
# all true


#checking if the pairwise matrix is symmetric and positive semi-definite
library(Matrix)
isSymmetric(cor_matrix)
eigenvalues <- eigen(cor_matrix)$values
all(eigenvalues >= 0)
#all true

# Compute distance matrix and perform hierarchical clustering
dist_matrix <- as.dist(1 - cor_matrix)

# Plot hierarchical clustering
hc_avg <- hclust(dist_matrix, method = "average")
save_plot("2016_03_clust_avg.jpg", {
  plot(hc_avg, main = "Hier. Clustering (complete obs. - average) 2016")
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
save_plot("2016_04_wss.jpg", print(wss_result))

# Display value for optimal k
# optimal = 2k 
print(wss_result$data$y[3])
# 0.114702


# Silhouette score
silhouette_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "silhouette",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2016_05_silhouette.jpg", print(silhouette_result))

# Display value for optimal k
# optimal = 2k 
print(silhouette_result$data$y[2])
# 0.6749513


# Gap Statistic
gap_stat_result <- fviz_nbclust(
  as.data.frame(as.matrix(dist_matrix)), 
  FUN = function(x, k) hcut(x, k, hc_method = "average"), 
  method = "gap_stat",
  k.max = 5,  # Limit the range of clusters (k) to avoid exceeding the size of the matrix
  verbose = FALSE
)

# Print the result
save_plot("2016_06_gap_stat.jpg", print(gap_stat_result))

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
# optimal = 2k Dunn index = 2.130003  

plot(
  k_values, dunn_scores,
  type = "b",                # connect points with lines
  xlab = "Number of Clusters (k)",
  ylab = "Dunn Index",
  main = "Dunn Index by Number of Clusters (2016)"
)

# Optionally highlight the optimal k
points(optimal_k, max(dunn_scores), col = "red", pch = 19, cex = 1.5)
text(optimal_k, max(dunn_scores), 
     labels = paste("Max at k =", optimal_k), 
     pos = 3, col = "red")

# Dunn Index visualization
# Filter out heights that produce only one cluster
valid_h <- h_seq[sapply(h_seq, function(h) length(unique(cutree(hc_avg, h = h))) > 1)]

# Compute Dunn Index only for valid heights
h_dunn <- sapply(valid_h, function(h) {
  clusters <- cutree(hc_avg, h = h)
  dunn(distance = dist_matrix, clusters)
})

# Plot only valid values
save_plot("2016_07_dunn.jpg", {
  plot(valid_h, h_dunn, xlab = "Height (h)", ylab = "Dunn index")
  grid()
})

#### Visualization with optimal number of ks ####

# cutoff for 2 ks 
k <- 2
h_for_k <- hc_avg$height[length(hc_avg$height) - (k - 1)]
print(h_for_k) 
# 0.1769235

# Plot the dendrogram with cutoff line
plot(hc_avg)
abline(h = 0.1769235, col = 'red')

# Dendrogram with rectangular cluster highlights
save_plot("2016_08_dend_high.jpg", {
  par(mar = c(5, 5, 5, 5), xpd = NA)  # Allow plotting outside the margins
  
  plot(hc_avg, 
       main = "Hierarchical Clustering Dendrogram (2016)",
       xlim = c(-10, length(hc_avg$order) + 10),  # Larger x-axis range
       hang = -1,                                 # Align leaves at the same baseline
       yaxt = "n",
       ylab = "Distance (1 - correlation)" )      # Suppress default y-axis ticks
  
  axis(2, at = seq(0, 0.4, 0.05), las = 1)         # Add custom y-axis ticks
  rect.hclust(hc_avg, k = 2, border = 2:4)         # Highlight clusters
})

# Dendrogram with colored branches using dendextend
avg_col_dend <- as.dendrogram(hc_avg)
avg_col_dend <- dendextend::color_branches(avg_col_dend, k = 2)
save_plot("2016_09_dend_col.jpg", {plot(avg_col_dend)})


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
save_plot("2016_10_clust_cpl.jpg", {
  plot(hc_cpl, main = "Hier. Clustering (complete obs. - complete 2016)")
})
# almost identical to average linkage

#### CORRELATION ANALYSIS - INNER JOIN ####
# Merge and filter data
target_year <- 2016
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


any(is.na(merged_data))
#no NAS > no difference between complete and pairwise obs


# Calculate the correlation matrix 
cor_matrix <- cor(numeric_data, use = "complete.obs")

print(cor_matrix)

# Results identical to full join complete


#####>>>>>>>>>TWO CLUSTERS SEEM TO BE THE OPTIMAL NUMBER OF CLUSTERS FOR 2016"


#### "BROADNESS" OF INDICES ####

# MCI

# Filter the data frame for the year 2016
df1_filtered <- df1 %>%
  filter(year == 2016) %>%
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

#$mean = 0.5158456

#$median = 0.5655437

#$sd = 0.2302482

#$min = -0.2106853

#$max = 0.9794592


# DSTRI

# Filter the data frame for the year 2016
df2_filtered <- df2 %>%
  filter(year == 2016) %>%
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

#$mean = 0.2845108

#$median = 0.2805817

#sd = 0.1116234

#$min = 0.05379507

#$max = 0.4892743

# EGDI 

# Filter the data frame for the year 2016
df3_filtered <- df3 %>%
  filter(year == 2016) %>%
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

#$mean = 0.4573335

#$median = 0.5923979

#$sd = 0.3863351

#$min = -0.3191774

#$max = 0.9621665

# DAI

# Filter the data frame for the year 2016
df4_filtered <- df4 %>%
  filter(year == 2016) %>%
  select(isocode, year, matches(".*\\(sub-idx\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
subidx_cols <- df4_filtered %>%
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

#$mean = 0.7294846

#$median = 0.673024

#$sd = 0.1484581

#$min = 0.6175402

#$max = 0.8978896

# IDI_Old

# Filter the data frame for the year 2016
df5_filtered <- df5 %>%
  filter(year == 2016) %>%
  select(isocode, year, matches(".*\\(idc\\)$"))  # Select only 'idc' columns

# Remove isocode and year column
idc_cols <- df5_filtered %>%
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

#$mean = 0.4573335

#$median = 0.5923979

#$sd = 0.3863351

#$min = -0.3191774

#$max = 0.9621665

