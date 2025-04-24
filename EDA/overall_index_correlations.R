#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

# Load Data Frames 
# Loading and cleaning individual data frames
df1 <- read_excel("MCI_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df2 <- read_excel("nri_cleaned_total.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df3 <- read_excel("IDI_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df4 <- read_excel("GTMI_cleaned_new.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df5 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df6 <- read_excel("Digital_STRI_inverted.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df7 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
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
df8 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df9 <- read_excel("AIPA_cleaned.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text")) %>%
  rename_with(~ tolower(trimws(.)))
df10 <- read_excel("DAIforweb_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df11 <- read_excel("DiGix_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
# No adding RLI values anymore! 

#### Correlation Matrices by Year ####

# List of data frames
# Put all 12 dfs into a list
dfs <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11)

#### Helper Function to Compute Correlation Matrix per Year ####

get_yearly_cor_matrix <- function(year) {
  # Merge all dataframes
  merged_data <- reduce(dfs, full_join, by = c("isocode", "year"))
  
  # Filter for current year
  merged_data <- merged_data %>% filter(year == !!year)
  
  # Remove columns and rows that are entirely NA
  merged_data <- merged_data[, colSums(!is.na(merged_data)) > 0]
  merged_data <- merged_data[rowSums(!is.na(merged_data)) > 0, ]
  
  # Pick only columns ending with (idx)
  idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)
  
  # Need at least 2 indices to calculate correlation
  if (length(idx_columns) < 2) return(NULL)
  
  numeric_data <- merged_data[, idx_columns, drop = FALSE]
  
  if (nrow(numeric_data) < 2) return(NULL)
  
  # Compute correlation matrices
  cor_complete <- cor(numeric_data, use = "complete.obs")  # only complete cases for each variable pair
  cor_pairwise <- cor(numeric_data, use = "pairwise.complete.obs")  # allow maximum available
  
  # Format complete.obs result
  cor_df_complete <- as.data.frame(cor_complete)
  cor_df_complete <- tibble::rownames_to_column(cor_df_complete, var = "Index")
  cor_df_complete <- mutate(cor_df_complete, Year = year) %>%
    select(Year, everything())
  
  # Format pairwise.complete.obs result (optional)
  cor_df_pairwise <- as.data.frame(cor_pairwise)
  cor_df_pairwise <- tibble::rownames_to_column(cor_df_pairwise, var = "Index")
  cor_df_pairwise <- mutate(cor_df_pairwise, Year = year) %>%
    select(Year, everything())
  
  return(list(complete = cor_df_complete, pairwise = cor_df_pairwise))
}

#### Run for All Years ####

# Find all years across all datasets
all_years <- unique(bind_rows(dfs)$year)
all_years <- sort(na.omit(all_years))

# Map function across years
correlation_results <- map(all_years, get_yearly_cor_matrix)

# Remove NULL results (years where no correlations can be calculated)
correlation_results <- compact(correlation_results)

# Separate complete and pairwise matrices
cor_complete_results <- map(correlation_results, "complete")
cor_pairwise_results <- map(correlation_results, "pairwise")

# Combine into big data frames
final_cor_matrix_complete <- bind_rows(cor_complete_results)
final_cor_matrix_pairwise <- bind_rows(cor_pairwise_results)

# View
print(final_cor_matrix_complete)
print(final_cor_matrix_pairwise)

# Save results if needed
write.csv(final_cor_matrix_complete, "index_correlation_overall_complete.csv", row.names = FALSE)
write.csv(final_cor_matrix_pairwise, "index_correlation_overall_pairwise.csv", row.names = FALSE)