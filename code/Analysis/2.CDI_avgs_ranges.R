# Clear previous data
rm(list = ls())


# Load data
cdi_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.CDI_participants.csv'), header=TRUE, stringsAsFactors=FALSE)
###################

#### remove participant 6944 
# Remove the row with ID_lab equal to "6944"
cdi_counts <- cdi_counts %>%
  filter(ID_lab != "6944")

# Function to calculate averages and ranges for a specific column
calculate_stats <- function(data, column) {
  list(
    T_avg = mean(data[[column]], na.rm = TRUE),
    T_range = diff(range(data[[column]], na.rm = TRUE)),
    Mono_avg = mean(data[[column]][data$Mono_Bi == "Mono"], na.rm = TRUE),
    Mono_range = diff(range(data[[column]][data$Mono_Bi == "Mono"], na.rm = TRUE)),
    Bi_avg = mean(data[[column]][data$Mono_Bi == "Bil"], na.rm = TRUE),
    Bi_range = diff(range(data[[column]][data$Mono_Bi == "Bil"], na.rm = TRUE))
  )
}

# Columns to calculate stats for
columns_to_calculate <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                          "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend", "skill_total_produce", 
                          "skill_concept_produce")

# Create a new data frame for the stats rows with columns corresponding to each statistic
stats_rows <- data.frame(
  ID_lab = c("T_avg", "T_range", "Mono_avg", "Mono_range", "Bi_avg", "Bi_range"),
  stringsAsFactors = FALSE
)

# Initialize other columns with NA
for (col in columns_to_calculate) {
  stats_rows[[col]] <- NA
}


# Calculate stats for each specified column
for (col in columns_to_calculate) {
  stats <- calculate_stats(cdi_counts, col)
  stats_rows[stats_rows$ID_lab == "T_avg", col] <- stats$T_avg
  stats_rows[stats_rows$ID_lab == "T_range", col] <- stats$T_range
  stats_rows[stats_rows$ID_lab == "Mono_avg", col] <- stats$Mono_avg
  stats_rows[stats_rows$ID_lab == "Mono_range", col] <- stats$Mono_range
  stats_rows[stats_rows$ID_lab == "Bi_avg", col] <- stats$Bi_avg
  stats_rows[stats_rows$ID_lab == "Bi_range", col] <- stats$Bi_range
}

# Ensure numeric columns are rounded to the nearest whole number and converted to integer
numeric_columns <- setdiff(names(stats_rows), "ID_lab")
stats_rows[numeric_columns] <- lapply(stats_rows[numeric_columns], function(x) as.integer(round(as.numeric(x))))

#rename
cdi_avg_rng <- stats_rows 

#save as CSV
write.csv(cdi_avg_rng, 
          file.path('raw-csv', 'Analysis', '10.CDI_averages.csv'),
          row.names=FALSE)






###################### delete????

# Function to calculate averages and ranges for a specific column
calculate_stats <- function(data, column) {
  list(
    T_avg = mean(data[[column]], na.rm = TRUE),
    T_range = diff(range(data[[column]], na.rm = TRUE)),
    Mono_avg = mean(data[[column]][data$bi_mono == "mono"], na.rm = TRUE),
    Mono_range = diff(range(data[[column]][data$bi_mono == "mono"], na.rm = TRUE)),
    Bi_avg = mean(data[[column]][data$bi_mono == "bi"], na.rm = TRUE),
    Bi_range = diff(range(data[[column]][data$bi_mono == "bi"], na.rm = TRUE))
  )
}

# Columns to calculate stats for
columns_to_calculate <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                          "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend")

# Create a new data frame for the stats rows with the same columns as CDI_AVG
stats_rows <- data.frame(
  ID_lab = c("T_avg", "T_range", "Mono_avg", "Mono_range", "Bi_avg", "Bi_range")
)

# Initialize other columns with NA
for (col in setdiff(names(CDI_AVG), "ID_lab")) {
  stats_rows[[col]] <- NA
}

# Calculate stats for each specified column
for (col in columns_to_calculate) {
  stats <- calculate_stats(CDI_AVG, col)
  stats_rows[stats_rows$ID_lab == "T_avg", col] <- stats$T_avg
  stats_rows[stats_rows$ID_lab == "T_range", col] <- stats$T_range
  stats_rows[stats_rows$ID_lab == "Mono_avg", col] <- stats$Mono_avg
  stats_rows[stats_rows$ID_lab == "Mono_range", col] <- stats$Mono_range
  stats_rows[stats_rows$ID_lab == "Bi_avg", col] <- stats$Bi_avg
  stats_rows[stats_rows$ID_lab == "Bi_range", col] <- stats$Bi_range
}

# Ensure numeric columns are rounded to the nearest whole number and converted to integer
numeric_columns <- setdiff(names(CDI_AVG), c("ID_lab", "bi_mono"))
CDI_AVG[numeric_columns] <- lapply(CDI_AVG[numeric_columns], function(x) as.integer(round(as.numeric(x))))

# Apply the same transformation to the stats_rows dataframe if needed
stats_rows[numeric_columns] <- lapply(stats_rows[numeric_columns], function(x) as.integer(round(as.numeric(x))))

# Add new rows to CDI_AVG
CDI_AVG <- rbind(CDI_AVG, stats_rows)

# Define the new order of column names
new_order <- c("ID_lab", "bi_mono", "skill_es_comprehend", "skill_eu_comprehend", 
               "skill_es_produce", "skill_eu_produce", "skill_total_comprehend", 
               "skill_concept_comprehend")

# Reorder the columns in the CDI_AVG dataframe
CDI_AVG <- CDI_AVG[, new_order]


#save as CSV
write.csv(CDI_AVG, 
          file.path('raw-csv', 'Analysis', '10.CDI_avgs_ranges.csv'),
          row.names=FALSE)


