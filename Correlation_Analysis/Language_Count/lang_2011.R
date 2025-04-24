#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)



# Load Data Frames 
# Loading and cleaning individual data frames
df1 <- read_excel("IDI_old_cleaned.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df2 <- read_excel("entropy_summary.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 

#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}

####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2011

# Merge all data frames
merged_data <- df1 %>%
  filter(year == target_year) %>%
  full_join(df2, by = "isocode")

# Extract index columns from digitization df
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "language_count")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("IDI", "language_count")

numeric_data$language_count <- log(numeric_data$language_count)


# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

                      # IDI language_count
# IDI             1.00000000    -0.07687545
# language_count -0.07687545     1.00000000

                      # IDI language_count
# IDI             1.00000000    -0.07687545
# language_count -0.07687545     1.00000000

#no significant difference in the results

write.csv(cor_matrix, "2011_1_cor_matrix.csv", row.names = TRUE)


# with just one value further analysis is not feasable 
