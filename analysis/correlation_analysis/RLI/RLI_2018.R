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
df7 <- read_excel("RLI_codes.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 



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
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "DSTRI",
                            "3i", "DiGiX", "EGDI", "EPI", "RLI")

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI        DSTRI        3i     DiGiX      EGDI         EPI         RLI
#MCI   1.0000000  0.36735124 0.9506877 0.8789449 0.9581549  0.78470726  0.14638089
#DSTRI  0.3673512  1.00000000 0.3279792 0.4364935 0.3865278  0.32524111 -0.08167332
#3i    0.9506877  0.32797923 1.0000000 0.8317894 0.9464928  0.82280179  0.14387868
#DiGiX 0.8789449  0.43649355 0.8317894 1.0000000 0.8872524  0.73248823  0.10196346
#EGDI  0.9581549  0.38652781 0.9464928 0.8872524 1.0000000  0.84182277  0.12352497
#EPI   0.7847073  0.32524111 0.8228018 0.7324882 0.8418228  1.00000000 -0.05255543
#RLI   0.1463809 -0.08167332 0.1438787 0.1019635 0.1235250 -0.05255543  1.00000000

            #MCI        DSTRI         3i      DiGiX       EGDI         EPI
#MCI   1.0000000  0.42232690 0.95904692 0.89344192 0.95649198  0.81071063
#DSTRI  0.4223269  1.00000000 0.44701085 0.45359687 0.42955848  0.40678674
#3i    0.9590469  0.44701085 1.00000000 0.85113312 0.93498645  0.80936560
#DiGiX 0.8934419  0.45359687 0.85113312 1.00000000 0.90892935  0.74993476
#EGDI  0.9564920  0.42955848 0.93498645 0.90892935 1.00000000  0.88321852
#EPI   0.8107106  0.40678674 0.80936560 0.74993476 0.88321852  1.00000000
#RLI   0.1419240 -0.05812011 0.05389253 0.06795258 0.04930459 -0.05770728
#RLI
#MCI    0.14192398
#DSTRI  -0.05812011
#3i     0.05389253
#DiGiX  0.06795258
#EGDI   0.04930459
#EPI   -0.05770728
#RLI    1.00000000

# no siginficant differences in the results

write.csv(cor_matrix, "2018_1_cor_matrix.csv", row.names = TRUE)
write.csv(cor_matrix_pair, "2018_2_cor_matrix_pair.csv", row.names = TRUE)



#### Correlation Plots ####
# Heatmap
plot <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                   type = "lower",  
                   colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap 2018") +
  
  # Customize the plot's appearance
  theme(
    axis.text.x = element_text(size = 8),   
    axis.text.y = element_text(size = 8),   
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  
  )

# Display the heatmap
print(plot)

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
pairs(
  ~ MCI + `3i` + DSTRI + DiGiX + EGDI + EPI + RLI, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Pairwise Correlations Plot with RLI (2018)" 
)

#### Summary Statistics #####


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

write.csv(summary_df, "2018_03_table_summary_pair.csv", row.names = TRUE)


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
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "3i",
                            "DSTRI", "DiGiX", "EGDI", "EPI", "RLI")


# Check for NAs
any(is.na(numeric_data))
# True > no difference between complete and pairwise obs

# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI          3i      DSTRI     DiGiX      EGDI         EPI         RLI
#MCI   1.0000000  0.36735124 0.9506877 0.8789449 0.9581549  0.78470726  0.14638089
#3i    0.3673512  1.00000000 0.3279792 0.4364935 0.3865278  0.32524111 -0.08167332
#DSTRI  0.9506877  0.32797923 1.0000000 0.8317894 0.9464928  0.82280179  0.14387868
#DiGiX 0.8789449  0.43649355 0.8317894 1.0000000 0.8872524  0.73248823  0.10196346
#EGDI  0.9581549  0.38652781 0.9464928 0.8872524 1.0000000  0.84182277  0.12352497
#EPI   0.7847073  0.32524111 0.8228018 0.7324882 0.8418228  1.00000000 -0.05255543
#RLI   0.1463809 -0.08167332 0.1438787 0.1019635 0.1235250 -0.05255543  1.00000000

            #MCI          3i      DSTRI       DiGiX       EGDI        EPI
#MCI   1.00000000  0.41544840 0.9506877 0.894680782 0.95831090  0.8142872
#3i    0.41544840  1.00000000 0.3279792 0.475485489 0.41369097  0.4184580
#DSTRI  0.95068774  0.32797923 1.0000000 0.831789428 0.94649279  0.8228018
#DiGiX 0.89468078  0.47548549 0.8317894 1.000000000 0.90945532  0.7962435
#EGDI  0.95831090  0.41369097 0.9464928 0.909455325 1.00000000  0.8700740
#EPI   0.81428719  0.41845800 0.8228018 0.796243479 0.87007398  1.0000000
#RLI   0.06482115 -0.05386932 0.1438787 0.005707723 0.03245185 -0.1164090
#RLI
#MCI    0.064821152
#3i    -0.053869316
#DSTRI   0.143878683
#DiGiX  0.005707723
#EGDI   0.032451852
#EPI   -0.116408951
#RLI    1.000000000

# no siginficant differences in the results