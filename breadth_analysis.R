#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(ggplot2)

# Load Data Frames
# Loading and cleaning individual data frames

# Indices with Indicator values
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
df3 <- read_excel("IDI_old_cleaned.xlsx", 
                  col_types = c("text", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric")) %>%
  rename_with(~ tolower(trimws(.)))
df4 <- read_excel("IDI_new_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df6 <- read_excel("GTMI_cleaned_new.xlsx", 
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
df7 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df8 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
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

# Indices with Sub-Index values available 
df9 <- read_excel("Digital_STRI_inverted.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df10 <- read_excel("AIPA_cleaned.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text")) %>%
  rename_with(~ tolower(trimws(.)))
df11 <- read_excel("DAIforweb_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))

#### Computation of correlations summary (Idc) ####


# Function to compute correlation summary
compute_correlation_summary <- function(data, year_label) {
  # Extract and clean index name
  idx_columns <- grep(".*\\(idx\\)$", colnames(data), value = TRUE)
  idx_name <- ifelse(length(idx_columns) > 0, gsub("[_\\s]*\\(idx\\)$", "", idx_columns[1]), NA)
  
  # Select only IDC columns
  idc_cols <- data %>%
    select(matches(".*\\(idc\\)$"))
  
  # Remove all-NA columns and rows
  cols_all_na <- colSums(is.na(idc_cols)) == nrow(idc_cols)
  rows_all_na <- rowSums(is.na(idc_cols)) == ncol(idc_cols)
  idc_cols <- idc_cols[, !cols_all_na]
  idc_cols <- idc_cols[!rows_all_na, ]
  
  if (nrow(idc_cols) > 1 && ncol(idc_cols) > 1) {
    cor_matrix <- cor(idc_cols, use = "pairwise.complete.obs")
    cor_values <- cor_matrix[upper.tri(cor_matrix, diag = FALSE)]
    
    return(data.frame(
      year = as.character(year_label),
      index_name = idx_name,
      mean = mean(cor_values, na.rm = TRUE),
      median = median(cor_values, na.rm = TRUE),
      sd = sd(cor_values, na.rm = TRUE),
      min = min(cor_values, na.rm = TRUE),
      max = max(cor_values, na.rm = TRUE)
    ))
  } else {
    return(NULL)
  }
}

# List of data frames to process
dataframes <- list(df1 = df1, df2 = df2, df3 = df3, df4 = df4, df5 = df5, df6 = df6, df7 = df7, df8 = df8)

# Collect all summaries
all_summaries <- list()

for (df in dataframes) {
  summary_list <- list()
  
  for (yr in 2004:2023) {
    df_year <- df %>%
      filter(year == yr) %>%
      select(isocode, year, matches(".*\\(idc\\)$"), matches(".*\\(idx\\)$"))
    
    if (nrow(df_year) > 0) {
      result <- compute_correlation_summary(df_year, yr)
      if (!is.null(result)) summary_list[[as.character(yr)]] <- result
    }
  }
  
  # Add full-dataset summary
  df_all <- df %>%
    select(isocode, year, matches(".*\\(idc\\)$"), matches(".*\\(idx\\)$"))
  overall_result <- compute_correlation_summary(df_all, "All Years")
  
  if (!is.null(overall_result)) {
    summary_list[["All Years"]] <- overall_result
  }
  
  # Bind and store results
  if (length(summary_list) > 0) {
    all_summaries[[length(all_summaries) + 1]] <- bind_rows(summary_list)
  }
}

# Combine all results
final_summary_df_idc <- bind_rows(all_summaries)

# Reorder and sort the final summary
final_summary_df_idc <- final_summary_df_idc %>%
  select(index_name, year, everything()) %>%
  mutate(year_numeric = ifelse(year == "All Years", Inf, as.numeric(year))) %>%
  arrange(index_name, year_numeric) %>%
  select(-year_numeric)

# Print the final summary table
cat("\n====================\n")
cat("Final Summary Table\n")
print(final_summary_df_idc)

View(final_summary_df_idc)

#### Computation of correlations summary (Sub-Idx) ####


# Function to compute correlation summary
compute_correlation_summary <- function(data, year_label) {
  # Extract and clean index name
  idx_columns <- grep(".*\\(idx\\)$", colnames(data), value = TRUE)
  idx_name <- ifelse(length(idx_columns) > 0, gsub("[_\\s]*\\(idx\\)$", "", idx_columns[1]), NA)
  
  # Select only Sub-Idx columns
  sub_idx_cols <- data %>%
    select(matches(".*\\(sub-idx\\)$"))
  
  # Remove all-NA columns and rows
  cols_all_na <- colSums(is.na(sub_idx_cols)) == nrow(sub_idx_cols)
  rows_all_na <- rowSums(is.na(sub_idx_cols)) == ncol(sub_idx_cols)
  sub_idx_cols <- sub_idx_cols[, !cols_all_na]
  sub_idx_cols <- sub_idx_cols[!rows_all_na, ]
  
  if (nrow(sub_idx_cols) > 1 && ncol(sub_idx_cols) > 1) {
    cor_matrix <- cor(sub_idx_cols, use = "pairwise.complete.obs")
    cor_values <- cor_matrix[upper.tri(cor_matrix, diag = FALSE)]
    
    return(data.frame(
      year = as.character(year_label),
      index_name = idx_name,
      mean = mean(cor_values, na.rm = TRUE),
      median = median(cor_values, na.rm = TRUE),
      sd = sd(cor_values, na.rm = TRUE),
      min = min(cor_values, na.rm = TRUE),
      max = max(cor_values, na.rm = TRUE)
    ))
  } else {
    return(NULL)
  }
}

# List of data frames to process
dataframes <- list(df9 = df9, df10 = df10, df11 = df11)

# Collect all summaries
all_summaries <- list()

for (df in dataframes) {
  summary_list <- list()
  
  for (yr in 2004:2023) {
    df_year <- df %>%
      filter(year == yr) %>%
      select(isocode, year, matches(".*\\(sub-idx\\)$"), matches(".*\\(idx\\)$"))
    
    if (nrow(df_year) > 0) {
      result <- compute_correlation_summary(df_year, yr)
      if (!is.null(result)) summary_list[[as.character(yr)]] <- result
    }
  }
  
  # Add full-dataset summary
  df_all <- df %>%
    select(isocode, year, matches(".*\\(sub-idx\\)$"), matches(".*\\(idx\\)$"))
  overall_result <- compute_correlation_summary(df_all, "All Years")
  
  if (!is.null(overall_result)) {
    summary_list[["All Years"]] <- overall_result
  }
  
  # Bind and store results
  if (length(summary_list) > 0) {
    all_summaries[[length(all_summaries) + 1]] <- bind_rows(summary_list)
  }
}

# Combine all results
final_summary_df_sub_idx <- bind_rows(all_summaries)

# Reorder and sort the final summary
final_summary_df_sub_idx <- final_summary_df_sub_idx %>%
  select(index_name, year, everything()) %>%
  mutate(year_numeric = ifelse(year == "All Years", Inf, as.numeric(year))) %>%
  arrange(index_name, year_numeric) %>%
  select(-year_numeric)

# Print the final summary table
cat("\n====================\n")
cat("Final Summary Table\n")
print(final_summary_df_sub_idx)

View(final_summary_df_sub_idx)

write.csv(final_summary_df_sub_idx, "final_summary_df_sub_idx.csv", row.names = TRUE)
