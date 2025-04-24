
#### Setup Environment and Load Data ####

# Load Required Libraries 
library(ggplot2)
library(dplyr)
library(rlang)
library(readxl)
library(stringr)
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
df8 <- read_excel("DAIforweb_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df9 <- read_excel("AIPA_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df10 <- read_excel("DiGix_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df11 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.)))


#### Function to create and save QQ plot ####

# Map index column names

index_mapping <- c(
  "mobile_connectivity_index_(idx)" = "MCI",
  "network_readiness_index_(idx)" = "NRI",
  "ict_development_index_(idx)" = "IDI",
  "govtech_maturity_index_(idx)" = "GTMI",
  "inclusive_internet_index_(idx)" = "3i",
  "inverted_services_trade_restrictiveness_index_(idx)" = "DSTRI",
  "e-government_index_(idx)" = "EGDI",
  "e-participation_index_(idx)" = "EPI",
  "ai-prepardness_index_(idx)" = "AIPI",
  "digital_adoption_index_(idx)" = "DAI",
  "digitization_index_(idx)" = "DiGiX"
)


create_and_save_qq_plot <- function(df, save_dir = "QQ_Plots") {
  # 1) Identify the index column
  idx_col <- grep(".*\\(idx\\)$", colnames(df), value = TRUE)
  if (length(idx_col) == 0) {
    warning("No index column found; skipping this dataframe.")
    return(NULL)
  }
  idx_col <- idx_col[1]
  
  # 2) Get renamed label
  index_label <- ifelse(idx_col %in% names(index_mapping), index_mapping[[idx_col]], idx_col)
  
  # 3) Prepare data
  df_qq <- df %>%
    filter(!is.na(year)) %>%          
    mutate(year = as.character(year))
  
  # 4) Check how many years
  unique_years <- sort(unique(df_qq$year))
  
  if (length(unique_years) > 1) {
    # If there are multiple years, add an "Overall"
    df_overall <- df_qq %>%
      mutate(year = "Overall")
    df_qq_combined <- bind_rows(df_qq, df_overall)
    
    year_levels <- c("Overall", unique_years)
  } else {
    # If only one year exists, skip "Overall"
    df_qq_combined <- df_qq
    year_levels <- unique_years
  }
  
  # 5) Set proper factor order
  df_qq_combined <- df_qq_combined %>%
    mutate(year = factor(year, levels = year_levels))
  
  # 6) Create plot
  p <- ggplot(df_qq_combined, aes(sample = !!sym(idx_col))) +
    stat_qq() +
    stat_qq_line(color = "blue") +
    facet_wrap(~ year, ncol = 3) +
    labs(
      title = paste("QQ Plots of", index_label, "by Year"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal(base_size = 16) +  # Set base font size bigger
    theme(
      strip.text = element_text(size = 18, face = "bold"),  # Bigger year labels
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 14)
    )
  
  # 7) Save plot
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  
  save_path <- file.path(save_dir, paste0("QQ_", index_label, ".jpg"))
  ggsave(filename = save_path, plot = p, width = 12, height = 9, dpi = 300)
}

#### Loop over all dataframes and create/save plots ####

walk(dfs, create_and_save_qq_plot)
