# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts.csv'), header=TRUE, stringsAsFactors=FALSE)

# Print all column names of the both_counts data frame
print(names(both_counts))

### CDI T1 descriptives 

# Subset the dataframe to include only the columns of interest
subset_df <- both_counts[, c("skill_es_comprehend", "skill_eu_comprehend", 
                             "skill_es_produce", "skill_eu_produce", 
                             "skill_total_comprehend", "skill_concept_comprehend", 
                             "skill_total_produce", "skill_concept_produce")]

# Calculate descriptive statistics using the summary() function
descriptive_stats <- summary(subset_df)

# Convert the summary statistics to a dataframe
CDI_T1_descriptives <- as.data.frame(descriptive_stats)

# Print the descriptive statistics dataframe
print(CDI_T1_descriptives)

#save as CSV
write.csv(CDI_T1_descriptives, 
          file.path('raw-csv', 'Analysis', 'Descriptives', 'CDI', 'T1',
                 'CDI_T1_descriptives.csv'),
          row.names=FALSE)

##### lena descriptives 
# Subset the dataframe to include only the columns of interest
subset_df_lena <- both_counts[, c("CT_COUNT_Avg", "CV_COUNT_Avg", "AWC_COUNT_Avg")]

# Calculate descriptive statistics for LENA variables
descriptive_stats_lena <- summary(subset_df_lena)

# Print descriptive statistics for LENA variables
cat("Descriptive Statistics for LENA Variables:\n")
print(descriptive_stats_lena)

# Save descriptive statistics to CSV
write.csv(descriptive_stats_lena, 
          file.path('raw-csv', 'Analysis', 'Descriptives', 'LENA_descriptives.csv'),
          row.names = FALSE)



### means of ages for CDI and LENA

# Calculate Mage and range for age_CDI
Mage_CDI <- mean(summary_dat$age_CDI, na.rm = TRUE)
range_CDI <- range(summary_dat$age_CDI, na.rm = TRUE)
range_CDI_diff <- diff(range_CDI)

# Calculate Mage and range for age_lena
Mage_lena <- mean(summary_dat$age_lena, na.rm = TRUE)
range_lena <- range(summary_dat$age_lena, na.rm = TRUE)
range_lena_diff <- diff(range_lena)

# Print results
cat("Mean age (Mage) for CDI:", Mage_CDI, "\n")
cat("Range for CDI:", range_CDI[1], "to", range_CDI[2], "\n")
cat("Range difference for CDI:", range_CDI_diff, "\n\n")

cat("Mean age (Mage) for LENA:", Mage_lena, "\n")
cat("Range for LENA:", range_lena[1], "to", range_lena[2], "\n")
cat("Range difference for LENA:", range_lena_diff, "\n")




# histograms

# Function to create histograms
create_histogram <- function(data, variable_name) {
  ggplot(data, aes(x = data[[variable_name]])) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", variable_name), x = variable_name, y = "Frequency") +
    theme_minimal()
}

# Create the directory if it doesn't exist
output_path <- file.path('raw-csv', 'Analysis', 'Descriptives', 'CDI', 'T1') 
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# CDI variables
cdi_variables <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                   "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend", 
                   "skill_total_produce", "skill_concept_produce")

# Create histograms for each CDI variable and save them
for (variable_name in cdi_variables) {
  histogram <- create_histogram(both_counts, variable_name)
  filename <- file.path(output_path, paste0("Histogram_", variable_name, ".png"))
  ggsave(filename, histogram, width = 8, height = 6)
}

# fixing 

# Function to create histograms with adjusted bin width
create_histogram <- function(data, variable_name) {
  ggplot(data, aes(x = data[[variable_name]])) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +  # Adjust bin width
    labs(title = paste("Histogram of", variable_name), x = variable_name, y = "Frequency") +
    theme_minimal()
}

# Create the directory if it doesn't exist
output_path <- file.path('raw-csv', 'Analysis', 'Descriptives', 'CDI', 'T1')
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# CDI variables
cdi_variables <- c("skill_concept_comprehend", "skill_eu_comprehend", "skill_total_comprehend")

# Create histograms with adjusted bin width for each CDI variable and save them
for (variable_name in cdi_variables) {
  histogram <- create_histogram(both_counts, variable_name)
  filename <- file.path(output_path, paste0("Histogram_", variable_name, ".png"))
  ggsave(filename, histogram, width = 8, height = 6)
}





#### LENA T1 Descriptives #####

#chart 

# Subset the dataframe to include only the columns of interest (LENA measures)
lena_subset_df <- both_counts[, c("CT_COUNT_Avg", "AWC_COUNT_Avg", "CV_COUNT_Avg")]

# Calculate descriptive statistics using the summary() function
lena_descriptive_stats <- summary(lena_subset_df)

# Convert the summary statistics to a dataframe
LENA_T1_descriptives <- as.data.frame(lena_descriptive_stats)

# Print the descriptive statistics dataframe
print(LENA_T1_descriptives)

#save as CSV
write.csv(LENA_T1_descriptives, 
          file.path('raw-csv', 'Analysis', 'Descriptives', 'LENA', 'LENA_T1_descriptives.csv'),
          row.names=FALSE)

# histograms

library(ggplot2)

# Function to create histograms with adjusted bin width
create_histogram <- function(data, variable_name) {
  ggplot(data, aes(x = data[[variable_name]])) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +  # Adjust bin width
    labs(title = paste("Histogram of", variable_name), x = variable_name, y = "Frequency") +
    theme_minimal()
}

# Create the directory if it doesn't exist
output_path <- file.path('raw-csv', 'Analysis', 'Descriptives', 'LENA')
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# LENA variables
lena_variables <- c("CT_COUNT_Avg", "AWC_COUNT_Avg", "CV_COUNT_Avg")

# Create histograms with adjusted bin width for each LENA variable and save them
for (variable_name in lena_variables) {
  histogram <- create_histogram(both_counts, variable_name)
  filename <- file.path(output_path, paste0("Histogram_", variable_name, ".png"))
  ggsave(filename, histogram, width = 8, height = 6)
}

#### trying more 

# Function to create histograms with adjusted bin width
create_histogram <- function(data, variable_name) {
  if (variable_name == "AWC_COUNT_Avg") {
    binwidth <- 30  # Adjust bin width for AWC_COUNT_Avg variable
  } else {
    binwidth <- 5  # Use default bin width for other variables
  }
  
  ggplot(data, aes(x = data[[variable_name]])) +
    geom_histogram(binwidth = binwidth, fill = "skyblue", color = "black") +  
    labs(title = paste("Histogram of", variable_name), x = variable_name, y = "Frequency") +
    theme_minimal()
}

# Create the directory if it doesn't exist
output_path <- file.path('raw-csv', 'Analysis', 'Descriptives', 'LENA')
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# LENA variables
lena_variables <- c("CT_COUNT_Avg", "AWC_COUNT_Avg", "CV_COUNT_Avg")

# Create histograms with adjusted bin width for each LENA variable and save them
for (variable_name in lena_variables) {
  histogram <- create_histogram(both_counts, variable_name)
  filename <- file.path(output_path, paste0("Histogram_", variable_name, ".png"))
  ggsave(filename, histogram, width = 8, height = 6)
}




