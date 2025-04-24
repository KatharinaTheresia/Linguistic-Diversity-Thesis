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
df3 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df4 <- read_excel("DiGix_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df5 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
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
df6 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df7 <- read_excel("entropy_summary.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 

#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}


####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2018
merged_data <- df1 %>%
  full_join(df2, by = c("isocode", "year")) %>%
  full_join(df3, by = c("isocode", "year")) %>%
  full_join(df4, by = c("isocode", "year")) %>%
  full_join(df5, by = c("isocode", "year")) %>%
  full_join(df6, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  full_join(df7, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename column
selected_columns <- c(idx_columns, "entropy")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "DSTRI",
                            "3i", "DiGiX", "EGDI", "EPI", "entropy")

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI        DSTRI        3i      DiGiX       EGDI        EPI
#MCI    1.0000000  0.3673512  0.9506877  0.8789449  0.9581549  0.7847073
#3i     0.3673512  1.0000000  0.3279792  0.4364935  0.3865278  0.3252411
#DSTRI   0.9506877  0.3279792  1.0000000  0.8317894  0.9464928  0.8228018
#DiGiX  0.8789449  0.4364935  0.8317894  1.0000000  0.8872524  0.7324882
#EGDI   0.9581549  0.3865278  0.9464928  0.8872524  1.0000000  0.8418228
#EPI    0.7847073  0.3252411  0.8228018  0.7324882  0.8418228  1.0000000
#H     -0.3700262 -0.1673925 -0.4822127 -0.1564805 -0.4248003 -0.4351973
              #H
#MCI   -0.3700262
#3i    -0.1673925
#DSTRI  -0.4822127
#DiGiX -0.1564805
#EGDI  -0.4248003
#EPI   -0.4351973
#H      1.0000000

            #MCI         DSTRI       3i      DiGiX       EGDI        EPI
#MCI    1.0000000  0.4223269  0.9590469  0.8934419  0.9564920  0.8107106
#3i     0.4223269  1.0000000  0.4470109  0.4535969  0.4295585  0.4067867
#DSTRI   0.9590469  0.4470109  1.0000000  0.8511331  0.9349865  0.8093656
#DiGiX  0.8934419  0.4535969  0.8511331  1.0000000  0.9089294  0.7499348
#EGDI   0.9564920  0.4295585  0.9349865  0.9089294  1.0000000  0.8832185
#EPI    0.8107106  0.4067867  0.8093656  0.7499348  0.8832185  1.0000000
#H     -0.4213854 -0.1647646 -0.4732264 -0.0777932 -0.3495580 -0.2193842
              #H
#MCI   -0.4213854
#3i    -0.1647646
#DSTRI  -0.4732264
#DiGiX -0.0777932
#EGDI  -0.3495580
#EPI   -0.2193842
#H      1.0000000


# significant differences across correlations 

write.csv(cor_matrix, "2018_1_cor_matrix.csv", row.names = TRUE)
write.csv(cor_matrix_pair, "2018_2_cor_matrix_pair.csv", row.names = TRUE)


#### Correlation Plots ####
# Heatmap
plot1 <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (pairwise complete obs.) 2018") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

plot2 <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (complete obs.) 2018") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

# Arrange plots
save_plot("2018_01_plot_heatmap.jpg", {
  grid.arrange(plot1, plot2, nrow = 2)
})

# Pairs plot 
# Define a custom panel for correlation coefficient
panel.cor = function(x, y, digits = 2, cex.cor = 1.2, alpha = 0.05, ...)
{
  par(usr = c(0, 1, 0, 1)) # axes range
  r = cor.test(x, y) # correlation test 
  if(r$p.value < alpha) star = "*" else star = "" # add star if significant 
  txt = paste(round(r$estimate, digits), star) # define text
  text(0.5, 0.5, txt, cex = cex.cor) # add text to box
}

# Generate the pairs plot
save_plot("2018_02_plot_pairs.jpg", pairs(
  ~ MCI + `3i` + DSTRI + DiGiX + EGDI + EPI + entropy, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Pairwise Correlations Plot with Entropy (2018)" 
))

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

write.csv(summary_df, "2018_03_table_summary_comp.csv", row.names = TRUE)

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

write.csv(summary_df, "2018_04_table_summary_pair.csv", row.names = TRUE)


#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data 
target_year <- 2018
merged_data <- df1 %>%
  inner_join(df2, by = c("isocode", "year")) %>%
  inner_join(df3, by = c("isocode", "year")) %>%
  inner_join(df4, by = c("isocode", "year")) %>%
  inner_join(df5, by = c("isocode", "year")) %>%
  inner_join(df6, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  inner_join(df7, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename column
selected_columns <- c(idx_columns, "entropy")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "3i",
                            "DSTRI", "DiGiX", "EGDI", "EPI", "entropy")


# Check for NAs
any(is.na(numeric_data))
# False > no difference between complete and pairwise obs

# Compute correlation matrix
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Display correlation matrix
print(cor_matrix)

# identical to full join complete
