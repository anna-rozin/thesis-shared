# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)
both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts.csv'), header=TRUE, stringsAsFactors=FALSE)


library(lubridate)


# Print all column names of the both_counts data frame
print(names(both_counts))

# Change the L1 value for participant 7255 to Eus if necessary
both_counts$L1[both_counts$ID_lab == 7255] <- "Eus"

# Update existing entries in both_counts for ID_lab 6925 and 7006B
both_counts[both_counts$ID_lab == "6925", c("Mono_Bil", "L1", "L2", "L1_p", "L2_p")] <- c("Bil", "Cas", "Eus", 52.1, 47.9)
both_counts[both_counts$ID_lab == "7006B", c("Mono_Bil", "L1", "L2", "L1_p", "L2_p")] <- c("Mono", "Eus", "Cas", 99.3, 0.7)


# Create new columns for L1 and L2 comprehension and production based on Cas (es) or Eus (eu)
both_counts$L1_comprehension <- ifelse(both_counts$L1 %in% c("es", "Cas"), both_counts$skill_es_comprehend, both_counts$skill_eu_comprehend)
both_counts$L2_comprehension <- ifelse(both_counts$L2 %in% c("es", "Cas"), both_counts$skill_es_comprehend, both_counts$skill_eu_comprehend)
both_counts$L1_production <- ifelse(both_counts$L1 %in% c("es", "Cas"), both_counts$skill_es_produce, both_counts$skill_eu_produce)
both_counts$L2_production <- ifelse(both_counts$L2 %in% c("es", "Cas"), both_counts$skill_es_produce, both_counts$skill_eu_produce)


# Calculate Total and Conceptual for L1 and L2 Comprehension
both_counts$L1_total_comprehension <- both_counts$skill_es_comprehend + both_counts$skill_eu_comprehend
both_counts$L2_total_comprehension <- both_counts$skill_es_comprehend + both_counts$skill_eu_comprehend

# Calculate Conceptual Comprehension as the sum of unique items comprehended in both languages
both_counts$L1_conceptual_comprehension <- both_counts$skill_concept_comprehend
both_counts$L2_conceptual_comprehension <- both_counts$skill_concept_comprehend

# Calculate Total and Conceptual for L1 and L2 Production
both_counts$L1_total_production <- both_counts$skill_es_produce + both_counts$skill_eu_produce
both_counts$L2_total_production <- both_counts$skill_es_produce + both_counts$skill_eu_produce

# Calculate Conceptual Production as the sum of unique items produced in both languages
both_counts$L1_conceptual_production <- both_counts$skill_concept_produce
both_counts$L2_conceptual_production <- both_counts$skill_concept_produce

# Subset the data based on Mono_Bil column
both_counts_mono <- subset(both_counts, Mono_Bil == "Mono")
both_counts_bil <- subset(both_counts, Mono_Bil == "Bil")

# Subset the dataframe to include only the columns of interest for L1 and L2
columns_of_interest_L1 <- c("L1_comprehension", "L1_production", "L1_conceptual_comprehension", "L1_conceptual_production", "L1_total_comprehension", "L1_total_production")
columns_of_interest_L2 <- c("L2_comprehension", "L2_production", "L2_conceptual_comprehension", "L2_conceptual_production", "L2_total_comprehension", "L2_total_production")

# Perform calculations separately for Mono and Bil groups
subset_L1_mono <- both_counts_mono[, columns_of_interest_L1]
subset_L2_mono <- both_counts_mono[, columns_of_interest_L2]
subset_L1_bil <- both_counts_bil[, columns_of_interest_L1]
subset_L2_bil <- both_counts_bil[, columns_of_interest_L2]

# Calculate descriptive statistics using the summary() function for Mono group
descriptive_stats_L1_mono <- summary(subset_L1_mono)
descriptive_stats_L2_mono <- summary(subset_L2_mono)

# Calculate descriptive statistics using the summary() function for Bil group
descriptive_stats_L1_bil <- summary(subset_L1_bil)
descriptive_stats_L2_bil <- summary(subset_L2_bil)

# Print the descriptive statistics for L1 and L2 for Mono group
print("Descriptive statistics for L1 (Mono):")
print(descriptive_stats_L1_mono)

print("Descriptive statistics for L2 (Mono):")
print(descriptive_stats_L2_mono)

# Print the descriptive statistics for L1 and L2 for Bil group
print("Descriptive statistics for L1 (Bil):")
print(descriptive_stats_L1_bil)

print("Descriptive statistics for L2 (Bil):")
print(descriptive_stats_L2_bil)



# Save the Mono group data
write.csv(both_counts_bil, file.path('raw-csv', 'Analysis', 'bil_both_counts_T2.csv'), row.names = FALSE)
write.csv(both_counts_mono, file.path('raw-csv', 'Analysis','mono_both_counts_T2.csv'), row.names = FALSE)

###################


