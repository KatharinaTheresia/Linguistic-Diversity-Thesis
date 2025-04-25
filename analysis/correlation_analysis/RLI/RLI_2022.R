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
df3 <- read_excel("GTMI_cleaned_new.xlsx", 
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
df4 <- read_excel("3i_meta_cleaned_total.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df5 <- read_excel("Digital_STRI_inverted.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df6 <- read_excel("EGDI_Iso.xlsx", col_types = c("numeric", 
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
df7 <- read_excel("EPI.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df8 <- read_excel("IDI_new_cleaned.xlsx") %>%
  rename_with(~ tolower(trimws(.)))
df9 <- read_excel("RLI_codes.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 


#### PLOT SAVING OPTIONS ####

save_plot <- function(filename, expr) {
  jpeg(filename, width = 1600, height = 1200, res = 300)
  force(expr)
  dev.off()
}


####CORRELATION ANALYSIS - FULL JOIN ####

# Merge and filter data 
target_year <- 2022
merged_data <- df1 %>%
  full_join(df2, by = c("isocode", "year")) %>%
  full_join(df3, by = c("isocode", "year")) %>%
  full_join(df4, by = c("isocode", "year")) %>%
  full_join(df5, by = c("isocode", "year")) %>%
  full_join(df6, by = c("isocode", "year")) %>%
  full_join(df7, by = c("isocode", "year")) %>%
  full_join(df8, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  full_join(df9, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "NRI", "GTMI", "3i", 
                            "DSTRI", "EGDI", "EPI", "IDI", "RLI")
# Compute correlation matrices
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Pairwise complete correlation matrix
cor_matrix_pair <- cor(numeric_data, use = "pairwise.complete.obs")  

# Display correlation matrices
print(cor_matrix)
print(cor_matrix_pair)

            #MCI          NRI      GTMI          3i       DSTRI        EGDI
#MCI   1.00000000  0.946242884 0.6693921  0.95770202  0.3509383 0.965878410
#NRI   0.94624288  1.000000000 0.5781493  0.88893679  0.3737481 0.926062156
#GTMI  0.66939205  0.578149348 1.0000000  0.71742138  0.2107991 0.716262997
#3i    0.95770202  0.888936790 0.7174214  1.00000000  0.2574635 0.953052857
#DSTRI  0.35093829  0.373748102 0.2107991  0.25746351  1.0000000 0.247422351
#EGDI  0.96587841  0.926062156 0.7162630  0.95305286  0.2474224 1.000000000
#EPI   0.82301318  0.836364514 0.6777123  0.83534962  0.2363217 0.885406509
#IDI   0.94681131  0.874765902 0.6568552  0.94356408  0.2195082 0.940053061
#RLI  -0.02007938 -0.007250981 0.1019058 -0.07777861 -0.0853161 0.004000145
            #EPI         IDI          RLI
#MCI   0.8230132  0.94681131 -0.020079376
#NRI   0.8363645  0.87476590 -0.007250981
#GTMI  0.6777123  0.65685524  0.101905777
#3i    0.8353496  0.94356408 -0.077778606
#DSTRI  0.2363217  0.21950821 -0.085316101
#EGDI  0.8854065  0.94005306  0.004000145
#EPI   1.0000000  0.78280799 -0.142847177
#IDI   0.7828080  1.00000000 -0.011793233
#RLI  -0.1428472 -0.01179323  1.000000000

          #MCI         NRI       GTMI         3i       DSTRI       EGDI
#MCI  1.0000000  0.93971333  0.7514288 0.94436206  0.3588194 0.95578353
#NRI  0.9397133  1.00000000  0.6665383 0.88578717  0.3877676 0.92722413
#GTMI 0.7514288  0.66653832  1.0000000 0.72351249  0.2669457 0.76238316
#3i   0.9443621  0.88578717  0.7235125 1.00000000  0.2537009 0.92641589
#DSTRI 0.3588194  0.38776761  0.2669457 0.25370089  1.0000000 0.30758113
#EGDI 0.9557835  0.92722413  0.7623832 0.92641589  0.3075811 1.00000000
#EPI  0.8127709  0.83413006  0.7942896 0.77780394  0.3306398 0.84625628
#IDI  0.9179455  0.83185303  0.5495716 0.91386377  0.2306121 0.89578953
#RLI  0.1357637 -0.01818434 -0.0389884 0.01687548 -0.0963375 0.05434958
            #EPI       IDI         RLI
#MCI   0.81277095 0.9179455  0.13576374
#NRI   0.83413006 0.8318530 -0.01818434
#GTMI  0.79428956 0.5495716 -0.03898840
#3i    0.77780394 0.9138638  0.01687548
#DSTRI  0.33063975 0.2306121 -0.09633750
#EGDI  0.84625628 0.8957895  0.05434958
#EPI   1.00000000 0.6231276 -0.04298171
#IDI   0.62312762 1.0000000  0.14906848
#RLI  -0.04298171 0.1490685  1.00000000

# results differ significantly for RLI vs MCI/EPI/IDI

write.csv(cor_matrix, "2022_1_cor_matrix.csv", row.names = TRUE)
write.csv(cor_matrix_pair, "2022_2_cor_matrix_pair.csv", row.names = TRUE)

#### Correlation Plots ####
# Heatmap
plot1 <- ggcorrplot(cor_matrix_pair, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (pairwise complete obs.) 2022") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

plot2 <- ggcorrplot(cor_matrix, hc.order = TRUE,  
                    type = "lower",  
                    colors = c("#6D9EC1", "yellow", "#E46726")) +
  ggtitle("Correlation Heatmap (complete obs.) 2022") +  # Add title
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Center title
  )

# Arrange plots
save_plot("2022_01_plot_heatmap.jpg", {
  grid.arrange(plot1, plot2, nrow = 2)
})

# Pairs plot 
# Define a custom panel for correlation coefficient
panel.cor = function(x, y, digits = 2, cex.cor = 1, alpha = 0.05, ...)
{
  par(usr = c(0, 1, 0, 1)) # axes range
  r = cor.test(x, y) # correlation test 
  if(r$p.value < alpha) star = "*" else star = "" # add star if significant 
  txt = paste(round(r$estimate, digits), star) # define text
  text(0.5, 0.5, txt, cex = cex.cor) # add text to box
}

# Generate the pairs plot
save_plot("2022_02_plot_pairs.jpg", pairs(
  ~ MCI + NRI + GTMI + `3i`+ DSTRI + EGDI + EPI + IDI + RLI, 
  data = numeric_data, 
  lower.panel = panel.cor,
  cex.labels = 1,
  main = "Pairwise Correlations Plot with RLI (2022)" 
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

write.csv(summary_df, "2022_03_table_summary_comp.csv", row.names = TRUE)

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

write.csv(summary_df, "2022_04_table_summary_pair.csv", row.names = TRUE)


#### CORRELATION ANALYSIS - INNER JOIN ####

# Merge and filter data


# Merge and filter data 
target_year <- 2022
merged_data <- df1 %>%
  inner_join(df2, by = c("isocode", "year")) %>%
  inner_join(df3, by = c("isocode", "year")) %>%
  inner_join(df4, by = c("isocode", "year")) %>%
  inner_join(df5, by = c("isocode", "year")) %>%
  inner_join(df6, by = c("isocode", "year")) %>%
  inner_join(df7, by = c("isocode", "year")) %>%
  inner_join(df8, by = c("isocode", "year")) %>%
  filter(year == target_year) %>%
  inner_join(df9, by = "isocode")

# Extract index columns from digitization dfs
idx_columns <- grep(".*\\(idx\\)$", colnames(merged_data), value = TRUE)

# Combine, subset and rename columns
selected_columns <- c(idx_columns, "rli")
numeric_data <- merged_data[, selected_columns]
colnames(numeric_data) <- c("MCI", "NRI", "GTMI", "3i", 
                            "DSTRI", "EGDI", "EPI", "IDI", "RLI")

# Check for NAs
any(is.na(numeric_data))
# False > no difference between complete and pairwise obs

# Compute correlation matrix
# Complete correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") 

# Display correlation matrix
print(cor_matrix)

# identical to full join complete
