# Clear previous data
rm(list = ls())



# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)
# Load data
cdi_avg_rng <- read.csv(file.path('raw-csv', 'Analysis', '10.CDI_averages.csv'), header=TRUE, stringsAsFactors=FALSE)
# Load data
cdi_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.CDI_participants_T2.csv'), header=TRUE, stringsAsFactors=FALSE)
# Load data
lena_counts <- read.csv(file.path('raw-csv', 'Summary', '1.Lena_summary.csv'), header=TRUE, stringsAsFactors=FALSE)


###################
both_counts <- merge(cdi_counts, lena_counts[, !names(lena_counts) %in% c("Recording_Gender", "Recording_DOB", "RecordingDate")], 
                     by.x = "ID_lab", by.y = "ExternalReferenceID", all = TRUE)


# Removing empty participants iwth no CDI data
both_counts <- both_counts[-c(16:24), ]


#save as CSV
write.csv(both_counts, 
          file.path('raw-csv', 'Analysis', '9.both_counts_T2.csv'),
          row.names=FALSE)



################### Delete 

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
columns_to_calculate <- c("CT_COUNT_sum", "AWC_COUNT_sum", "CV_COUNT_sum")

# Create a new data frame for the stats rows with the name lena_avg_rng
lena_avg_rng <- data.frame(
  ID_lab = c("T_avg", "T_range", "Mono_avg", "Mono_range", "Bi_avg", "Bi_range")
)

# Initialize other columns with NA
for (col in columns_to_calculate) {
  lena_avg_rng[[col]] <- NA
}

# Calculate stats for each specified column
for (col in columns_to_calculate) {
  stats <- calculate_stats(both_counts, col)
  lena_avg_rng[lena_avg_rng$ID_lab == "T_avg", col] <- stats$T_avg
  lena_avg_rng[lena_avg_rng$ID_lab == "T_range", col] <- stats$T_range
  lena_avg_rng[lena_avg_rng$ID_lab == "Mono_avg", col] <- stats$Mono_avg
  lena_avg_rng[lena_avg_rng$ID_lab == "Mono_range", col] <- stats$Mono_range
  lena_avg_rng[lena_avg_rng$ID_lab == "Bi_avg", col] <- stats$Bi_avg
  lena_avg_rng[lena_avg_rng$ID_lab == "Bi_range", col] <- stats$Bi_range
}

# Ensure numeric columns are rounded to the nearest whole number and converted to integer
numeric_columns <- setdiff(names(lena_avg_rng), "ID_lab")
lena_avg_rng[numeric_columns] <- lapply(lena_avg_rng[numeric_columns], function(x) as.integer(round(as.numeric(x))))

# Print lena_avg_rng to verify
print(lena_avg_rng)

#save as CSV
write.csv(lena_counts, 
          file.path('raw-csv', 'Analysis', '10.lena_averages.csv'),
          row.names=FALSE)

##### combine

# Merge the data frames based on the ID_lab column, specifying suffixes
combined_avg_rng <- merge(lena_avg_rng, cdi_avg_rng, by = "ID_lab", all = TRUE, suffixes = c(".lena", ".cdi"))


#save as CSV
write.csv(combined_avg_rng, 
          file.path('raw-csv', 'Analysis', '10.both_averages.csv'),
          row.names=FALSE)




