# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_both.csv'), header=TRUE, stringsAsFactors=FALSE)
both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts.csv'), header=TRUE, stringsAsFactors=FALSE)

# Print all column names of the both_counts data frame
print(names(both_counts))

# Change the L1 value for participant 7255 to Eus if necessary
both_counts$L1[both_counts$ID_lab == 7255] <- "Eus"

# Create new columns for L1 and L2 comprehension and production based on Cas (es) or Eus (eu)
both_counts$L1_comprehension <- ifelse(both_counts$L1 %in% c("es", "Cas"), both_counts$skill_es_comprehend, both_counts$skill_eu_comprehend)
both_counts$L2_comprehension <- ifelse(both_counts$L2 %in% c("es", "Cas"), both_counts$skill_es_comprehend, both_counts$skill_eu_comprehend)
both_counts$L1_production <- ifelse(both_counts$L1 %in% c("es", "Cas"), both_counts$skill_es_produce, both_counts$skill_eu_produce)
both_counts$L2_production <- ifelse(both_counts$L2 %in% c("es", "Cas"), both_counts$skill_es_produce, both_counts$skill_eu_produce)

# Calculate Total and Conceptual for L1 and L2 Comprehension
both_counts$L1_total_comprehension <- both_counts$skill_es_comprehend + both_counts$skill_eu_comprehend
both_counts$L2_total_comprehension <- both_counts$skill_es_comprehend + both_counts$skill_eu_comprehend

both_counts$L1_conceptual_comprehension <- (both_counts$skill_concept_comprehend / 2)  # Assuming concept_comprehend is the average of es and eu
both_counts$L2_conceptual_comprehension <- (both_counts$skill_concept_comprehend / 2)  # Assuming concept_comprehend is the average of es and eu

# Calculate Total and Conceptual for L1 and L2 Production
both_counts$L1_total_production <- both_counts$skill_es_produce + both_counts$skill_eu_produce
both_counts$L2_total_production <- both_counts$skill_es_produce + both_counts$skill_eu_produce

both_counts$L1_conceptual_production <- (both_counts$skill_concept_produce / 2)  # Assuming concept_produce is the average of es and eu
both_counts$L2_conceptual_production <- (both_counts$skill_concept_produce / 2)  # Assuming concept_produce is the average of es and eu

# Subset the dataframe to include only the columns of interest for L1 and L2
columns_of_interest_L1 <- c("L1_comprehension", "L1_production", "L1_conceptual_comprehension", "L1_conceptual_production", "L1_total_comprehension", "L1_total_production")
columns_of_interest_L2 <- c("L2_comprehension", "L2_production", "L2_conceptual_comprehension", "L2_conceptual_production", "L2_total_comprehension", "L2_total_production")

subset_L1 <- both_counts[, columns_of_interest_L1]
subset_L2 <- both_counts[, columns_of_interest_L2]

# Calculate descriptive statistics using the summary() function
descriptive_stats_L1 <- summary(subset_L1)
descriptive_stats_L2 <- summary(subset_L2)

# Print the descriptive statistics for L1 and L2
print("Descriptive statistics for L1:")
print(descriptive_stats_L1)

print("Descriptive statistics for L2:")
print(descriptive_stats_L2)

#save as CSV
write.csv(both_counts, 
          file.path('raw-csv', 'Analysis', '9.both_counts_L1L2.csv'),
          row.names=FALSE)
