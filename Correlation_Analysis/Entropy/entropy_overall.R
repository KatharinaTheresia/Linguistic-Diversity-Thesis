#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

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
df5 <- read_excel("GTMI_cleaned_new.xlsx", 
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
df6 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df7 <- read_excel("Digital_STRI_inverted.xlsx") %>%
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
df9 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df10 <- read_excel("AIPA_cleaned.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text")) %>%
  rename_with(~ tolower(trimws(.)))
df11 <- read_excel("DAIforweb_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df12 <- read_excel("DiGix_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 

df13 <- read_excel("entropy_codes.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 


# Add entropy values to all digitization dfs 

# Step 1: Add df12's value column to each df based on isocode
add_df13_value <- function(df) {
  df %>%
    left_join(df13, by = "isocode") # Match on isocode to add the value column
}

# Apply the function to each df
df1 <- add_df13_value(df1)
df2 <- add_df13_value(df2)
df3 <- add_df13_value(df3)
df4 <- add_df13_value(df4)
df5 <- add_df13_value(df5)
df6 <- add_df13_value(df6)
df7 <- add_df13_value(df7)
df8 <- add_df13_value(df8)
df9 <- add_df13_value(df9)
df10 <- add_df13_value(df10)
df11 <- add_df13_value(df11)
df12 <- add_df13_value(df12)

#### Correlation Analysis over all years####

# List of data frames
dfs <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

# Initialize an empty data frame to store results
cor_results <- data.frame(Index = character(), Year = numeric(), Correlation = numeric(), PValue = numeric(), Significance = character(), stringsAsFactors = FALSE)

# Loop through each data frame
for (df in dfs) {
  # Select index columns (match pattern for index columns)
  index_cols <- grep(".*\\(idx\\)$", names(df), value = TRUE)
  
  # Check if the 'year' column exists
  if(!"year" %in% names(df)) {
    warning("No 'year' column found in this data frame; skipping.")
    next
  }
  
  # Loop through each unique year in the data frame
  for (yr in unique(df$year)) {
    df_year <- subset(df, year == yr)
    
    # Compute correlations between each index column and entropy for the current year
    for (col in index_cols) {
      # Ensure there are enough observations for the correlation computation
      if (sum(!is.na(df_year[[col]]) & !is.na(df_year$entropy)) > 1) {
        test <- cor.test(df_year[[col]], df_year$entropy, use = "pairwise.complete.obs")
        cor_val <- test$estimate
        p_val <- test$p.value
        
        # Determine significance level (adjust the threshold if necessary)
        sig <- ifelse(p_val < 0.05, "*", "")
        
        # Append the results
        cor_results <- rbind(cor_results, data.frame(Index = col, Year = yr, Correlation = cor_val, PValue = p_val, Significance = sig, stringsAsFactors = FALSE))
      }
    }
  }
}

# Rename the indices 

mapping <- c(
  "mobile_connectivity_index_(idx)" = "MCI",
  "network_readiness_index_(idx)" = "NRI",
  "old_ict_development_index_(idx)" = "IDI_old",
  "new_ict_development_index_(idx)" = "IDI_new",
  "govtech_maturity_index_(idx)" = "GTMI",
  "inclusive_internet_index_(idx)" = "3i",
  "inverted_services_trade_restrictiveness_index_(idx)" = "DSTRI",
  "e-government_index_(idx)" = "EGDI",
  "e-participation_index_(idx)" = "EPI",
  "ai-prepardness_index_(idx)" = "AIPI",
  "digital_adoption_index_(idx)" = "DAI",
  "digitization_index_(idx)" = "DiGiX"
)

cor_results <- cor_results %>%
  mutate(Index = recode(Index, !!!mapping))

print(cor_results)

#### Plot a Heatmap with significance ####


# Complete the grid so that every Year-Index combination is represented
heatmap_df <- cor_results %>%
  complete(Year, Index)

# Create a label column that includes the correlation value and a star if significant,
# or "NA" if the correlation is missing.
heatmap_df <- heatmap_df %>%
  mutate(Label = ifelse(is.na(Correlation), 
                        "NA", 
                        ifelse(Significance == "*", 
                               paste0(round(Correlation, 2), " *"), 
                               paste0(round(Correlation, 2)))))

# Plot the heatmap: 
ggplot(heatmap_df, aes(x = Index, y = as.factor(Year), fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Label), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, na.value = "grey90") +
  labs(title = "Correlations between Entropy and Digitization Indices Over Time",
       x = "Index", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
