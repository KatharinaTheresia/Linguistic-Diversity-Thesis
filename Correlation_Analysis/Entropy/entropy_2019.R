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
df3 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df4 <- read_excel("Digital_STRI_inverted.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df5 <- read_excel("DiGix_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df6 <- read_excel("entropy_summary.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 

#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}

####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data
target_year <- 2019
merged_data <- df1 %>%
  full_join(df2, by = c("isocode", "year")) %>%
  full_join(df3, by = c("isocode", "year")) %>%
  full_join(df4, by = c("isocode", "year")) %>%
  full_join(df5, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  full_join(df6, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "entropy")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "NRI",
                            "3i", "DSTRI","DiGiX", "entropy")
# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI         NRI         3i       DSTRI      DiGiX           H
#MCI    1.0000000  0.27211526  0.9462313  0.3610799  0.8823045 -0.34384806
#NRI    0.2721153  1.00000000  0.2987107  0.0713433  0.1916121 -0.07398963
#3i     0.9462313  0.29871071  1.0000000  0.3024732  0.8443128 -0.43632146
#DSTRI   0.3610799  0.07134330  0.3024732  1.0000000  0.3989664 -0.21069360
#DiGiX  0.8823045  0.19161207  0.8443128  0.3989664  1.0000000 -0.11984282
#H     -0.3438481 -0.07398963 -0.4363215 -0.2106936 -0.1198428  1.00000000
#
            #MCI        NRI         3i       DSTRI       DiGiX           H
#MCI    1.0000000  0.2640589  0.9631127  0.3552469  0.90116973 -0.41834846
#NRI    0.2640589  1.0000000  0.2260904  0.1660281  0.16557554 -0.08454950
#3i     0.9631127  0.2260904  1.0000000  0.2277836  0.86324726 -0.48372435
#DSTRI   0.3552469  0.1660281  0.2277836  1.0000000  0.45967250 -0.10579045
#DiGiX  0.9011697  0.1655755  0.8632473  0.4596725  1.00000000 -0.07726373
#H     -0.4183485 -0.0845495 -0.4837243 -0.1057904 -0.07726373  1.00000000

# no significant difference in the results

write.csv(cor_matrix, "2019_1_cor_matrix.csv", row.names = TRUE)
write.csv(cor_matrix_pair, "2019_2_cor_matrix_pair.csv", row.names = TRUE)


#### Correlation Plots ####
# Heatmap
plot <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                   type = "lower",  
                   colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap 2019") +
  
  # Customize the plot's appearance
  theme(
    axis.text.x = element_text(size = 8),   
    axis.text.y = element_text(size = 8),   
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  
  )

# Display the heatmap
save_plot("2019_01_plot_heatmap.jpg", print(plot))


# Pairs plot 
# Define a custom panel for correlation coefficient
panel.cor = function(x, y, digits = 2, cex.cor = 1.5, alpha = 0.05, ...)
{
  par(usr = c(0, 1, 0, 1)) # axes range
  r = cor.test(x, y) # correlation test 
  if(r$p.value < alpha) star = "*" else star = "" # add star if significant 
  txt = paste(round(r$estimate, digits), star) # define text
  text(0.5, 0.5, txt, cex = cex.cor) # add text to box
}
# Generate the pairs plot
save_plot("2019_02_plot_pairs.jpg", pairs(
  ~ MCI + NRI + `3i` + DSTRI + DiGiX + entropy, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Pairwise Correlations Plot with Entropy (2019)" 
))



#### Summary Statistics #####
# Inferential Metrics of Correlation Analysis 

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

write.csv(summary_df, "2019_03_table_summary.csv", row.names = TRUE)

#### CORRELATION ANALYSIS - INNER JOIN ####


# Merge and filter data 
target_year <- 2019
merged_data <- df1 %>%
  inner_join(df2, by = c("isocode", "year")) %>%
  inner_join(df3, by = c("isocode", "year")) %>%
  inner_join(df4, by = c("isocode", "year")) %>%
  inner_join(df5, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  inner_join(df6, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "entropy")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "NRI",
                            "3i", "DSTRI","DiGiX", "entropy")

# Check for NAs
any(is.na(numeric_data))
# True > difference between complete and pairwise obs


# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI         NRI         3i       DSTRI      DiGiX           H
#MCI    1.0000000  0.27211526  0.9462313  0.3610799  0.8823045 -0.34384806
#NRI    0.2721153  1.00000000  0.2987107  0.0713433  0.1916121 -0.07398963
#3i     0.9462313  0.29871071  1.0000000  0.3024732  0.8443128 -0.43632146
#DSTRI   0.3610799  0.07134330  0.3024732  1.0000000  0.3989664 -0.21069360
#DiGiX  0.8823045  0.19161207  0.8443128  0.3989664  1.0000000 -0.11984282
#H     -0.3438481 -0.07398963 -0.4363215 -0.2106936 -0.1198428  1.00000000

            #MCI         NRI         3i       DSTRI       DiGiX           H
#MCI    1.0000000  0.33360068  0.9462313  0.4729656  0.90210146 -0.27929599
#NRI    0.3336007  1.00000000  0.2987107  0.2084823  0.26397733 -0.08129574
#3i     0.9462313  0.29871071  1.0000000  0.3024732  0.84431284 -0.43632146
#DSTRI   0.4729656  0.20848227  0.3024732  1.0000000  0.48355778 -0.21909974
#DiGiX  0.9021015  0.26397733  0.8443128  0.4835578  1.00000000 -0.07409993
#H     -0.2792960 -0.08129574 -0.4363215 -0.2190997 -0.07409993  1.00000000

# no significant different in the results

