#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(gridExtra)


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
df4 <- read_excel("RLI_codes.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 


####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2015
merged_data <- df1 %>%
  full_join(df2, by = c("isocode", "year")) %>%
  full_join(df3, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  full_join(df4, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "DSTRI","IDI", "RLI")

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI       DSTRI       IDI        RLI
#MCI  1.00000000 0.53342914 0.9513504 0.06427515
#DSTRI 0.53342914 1.00000000 0.5458952 0.03284551
#IDI  0.95135042 0.54589519 1.0000000 0.11484411
#RLI  0.06427515 0.03284551 0.1148441 1.00000000

          #MCI       DSTRI       IDI        RLI
#MCI  1.0000000 0.53666845 0.9453847 0.13182583
#DSTRI 0.5366685 1.00000000 0.5458952 0.01571801
#IDI  0.9453847 0.54589519 1.0000000 0.12077312
#RLI  0.1318258 0.01571801 0.1207731 1.00000000

# significant difference for RLI vs. MCI

write.csv(cor_matrix, "2015_1_cor_matrix.csv", row.names = TRUE)
write.csv(cor_matrix_pair, "2015_2_cor_matrix_pair.csv", row.names = TRUE)

#### Correlation Plots ####
# Heatmap
plot1 <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (pairwise complete obs.) 2015") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

plot2 <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (complete obs.) 2015") +  # Add title
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
  ~ MCI + DSTRI + IDI + RLI, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Pairwise Correlations Plot with RLI (2015)" 
)

#### Summary Statistics #####

# Inferential Metrics of Correlation Analysis - complete

# Helper function: Categorize effect strength based on correlation
effect_strength <- function(correlation) {
  if (abs(correlation) < 0.1) {
    return("Very Small")
  } else if (abs(correlation) < 0.3) {
    return("Small")
  } else if (abs(correlation) < 0.5) {
    return("Medium")
  } else {
    return("Large")
  }
}

# Helper function: Determine significance based on confidence intervals
is_significant <- function(ci_lower, ci_upper) {
  if (ci_lower > 0 | ci_upper < 0) {  # If CI does not include 0
    return("*")
  } else {
    return("")
  }
}

# Helper function: Determine the direction of the correlation
correlation_direction <- function(correlation) {
  if (correlation > 0) {
    return("Positive")
  } else {
    return("Negative")
  }
}

# Helper function: Compute confidence intervals for correlations
cor_ci <- function(x, y) {
  test <- cor.test(x, y, use = "complete.obs")
  return(c(lower = test$conf.int[1], upper = test$conf.int[2]))
}

# Main workflow
# Compute correlation matrix
# Helper function: Format p-value for small numbers
format_p_value <- function(p_value) {
  if (p_value < 1e-7) {
    return(format(p_value, scientific = TRUE, digits = 3))
  } else {
    return(round(p_value, 7))
  }
}

# Function to calculate p-value for correlation
cor_p_value <- function(x, y) {
  cor_test <- cor.test(x, y)  # Perform the correlation test
  return(cor_test$p.value)   # Extract and return the p-value
}

# Existing code
# Initialize an empty data frame for the summary
summary_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  Correlation = numeric(),
  P_Value = character(),  # Changed to character to store scientific notation
  EffectStrength = character(),
  Significance = character(),
  Direction = character(),
  stringsAsFactors = FALSE
)

# Loop through the upper triangle of the correlation matrix
for (i in 1:(nrow(cor_matrix) - 1)) {
  for (j in (i + 1):ncol(cor_matrix)) {
    # Compute p-value
    p_value <- cor_p_value(numeric_data[[i]], numeric_data[[j]])
    
    # Add a row to the summary data frame
    summary_df <- rbind(
      summary_df,
      data.frame(
        Variable1 = colnames(numeric_data)[i],
        Variable2 = colnames(numeric_data)[j],
        Correlation = round(cor_matrix[i, j], 5),
        P_Value = format_p_value(p_value),  # Use scientific notation for small p-values
        EffectStrength = effect_strength(cor_matrix[i, j]),
        Significance = ifelse(p_value < 0.05, "*", ""),
        Direction = correlation_direction(cor_matrix[i, j]),
        row.names = NULL  # Ensure no row names are added
      )
    )
  }
}

# Display the Inferential Metrics
print(summary_df, row.names = FALSE)

write.csv(summary_df, "2015_03_table_summary_comp.csv", row.names = TRUE)


# Inferential Metrics of Correlation Analysis - pairwise

# Helper function: Categorize effect strength based on correlation
effect_strength <- function(correlation) {
  if (abs(correlation) < 0.1) {
    return("Very Small")
  } else if (abs(correlation) < 0.3) {
    return("Small")
  } else if (abs(correlation) < 0.5) {
    return("Medium")
  } else {
    return("Large")
  }
}

# Helper function: Determine significance based on confidence intervals
is_significant <- function(ci_lower, ci_upper) {
  if (ci_lower > 0 | ci_upper < 0) {  # If CI does not include 0
    return("*")
  } else {
    return("")
  }
}

# Helper function: Determine the direction of the correlation
correlation_direction <- function(correlation) {
  if (correlation > 0) {
    return("Positive")
  } else {
    return("Negative")
  }
}

# Helper function: Compute confidence intervals for correlations
cor_ci <- function(x, y) {
  test <- cor.test(x, y, use = "pairwise.complete.obs")
  return(c(lower = test$conf.int[1], upper = test$conf.int[2]))
}

# Main workflow
# Compute correlation matrix
# Helper function: Format p-value for small numbers
format_p_value <- function(p_value) {
  if (p_value < 1e-7) {
    return(format(p_value, scientific = TRUE, digits = 3))
  } else {
    return(round(p_value, 7))
  }
}

# Function to calculate p-value for correlation
cor_p_value <- function(x, y) {
  cor_test <- cor.test(x, y)  # Perform the correlation test
  return(cor_test$p.value)   # Extract and return the p-value
}

# Existing code
# Initialize an empty data frame for the summary
summary_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  Correlation = numeric(),
  P_Value = character(),  # Changed to character to store scientific notation
  EffectStrength = character(),
  Significance = character(),
  Direction = character(),
  stringsAsFactors = FALSE
)

# Loop through the upper triangle of the correlation matrix
for (i in 1:(nrow(cor_matrix_pair) - 1)) {
  for (j in (i + 1):ncol(cor_matrix_pair)) {
    # Compute p-value
    p_value <- cor_p_value(numeric_data[[i]], numeric_data[[j]])
    
    # Add a row to the summary data frame
    summary_df <- rbind(
      summary_df,
      data.frame(
        Variable1 = colnames(numeric_data)[i],
        Variable2 = colnames(numeric_data)[j],
        Correlation = round(cor_matrix_pair[i, j], 5),
        P_Value = format_p_value(p_value),  # Use scientific notation for small p-values
        EffectStrength = effect_strength(cor_matrix_pair[i, j]),
        Significance = ifelse(p_value < 0.05, "*", ""),
        Direction = correlation_direction(cor_matrix_pair[i, j]),
        row.names = NULL  # Ensure no row names are added
      )
    )
  }
}

# Display the Inferential Metrics
print(summary_df, row.names = FALSE)

write.csv(summary_df, "2015_04_table_summary_pair.csv", row.names = TRUE)

#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data 
target_year <- 2015
merged_data <- df1 %>%
  inner_join(df2, by = c("isocode", "year")) %>%
  inner_join(df3, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  inner_join(df4, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "DSTRI","IDI", "RLI")

# Check for NAs
any(is.na(numeric_data))
# False > no difference between complete and pairwise obs

# Compute correlation matrix
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Display correlation matrix
print(cor_matrix)

# identical to full join complete
