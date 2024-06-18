# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure the skill columns are binary
summary_dat$skill_eu_comprehend <- as.numeric(as.character(summary_dat$skill_eu_comprehend))
summary_dat$skill_es_comprehend <- as.numeric(as.character(summary_dat$skill_es_comprehend))
summary_dat$skill_eu_produce <- as.numeric(as.character(summary_dat$skill_eu_produce))
summary_dat$skill_es_produce <- as.numeric(as.character(summary_dat$skill_es_produce))

# IDENTIFY  bilingual participants
bilinguals <- summary_dat[!is.na(summary_dat$skill_eu_comprehend) & !is.na(summary_dat$skill_es_comprehend), ]

####Calculate the total vocabulary by 
###summing the known words in each language for comprehension and production:

# Total comprehension vocabulary
bilinguals$total_comprehend <- bilinguals$skill_eu_comprehend + bilinguals$skill_es_comprehend

# Total production vocabulary
bilinguals$total_produce <- bilinguals$skill_eu_produce + bilinguals$skill_es_produce


#### To calculate the conceptual vocabulary, 
####we need to count unique concepts known in either language:
# Conceptual comprehension vocabulary
bilinguals$conceptual_comprehend <- rowSums(bilinguals[, c("skill_eu_comprehend", "skill_es_comprehend")] > 0)

# Conceptual production vocabulary
bilinguals$conceptual_produce <- rowSums(bilinguals[, c("skill_eu_produce", "skill_es_produce")] > 0)


####
# Aggregate data for total comprehension
average_total_comprehend <- mean(bilinguals$total_comprehend, na.rm = TRUE)
range_total_comprehend <- diff(range(bilinguals$total_comprehend, na.rm = TRUE))

# Aggregate data for conceptual comprehension
average_conceptual_comprehend <- mean(bilinguals$conceptual_comprehend, na.rm = TRUE)
range_conceptual_comprehend <- diff(range(bilinguals$conceptual_comprehend, na.rm = TRUE))

# Aggregate data for total production
average_total_produce <- mean(bilinguals$total_produce, na.rm = TRUE)
range_total_produce <- diff(range(bilinguals$total_produce, na.rm = TRUE))

# Aggregate data for conceptual production
average_conceptual_produce <- mean(bilinguals$conceptual_produce, na.rm = TRUE)
range_conceptual_produce <- diff(range(bilinguals$conceptual_produce, na.rm = TRUE))

# Display results
cat("Average Total Comprehension Vocabulary:", average_total_comprehend, "\n")
cat("Range of Total Comprehension Vocabulary:", range_total_comprehend, "\n")
cat("Average Conceptual Comprehension Vocabulary:", average_conceptual_comprehend, "\n")
cat("Range of Conceptual Comprehension Vocabulary:", range_conceptual_comprehend, "\n")

cat("Average Total Production Vocabulary:", average_total_produce, "\n")
cat("Range of Total Production Vocabulary:", range_total_produce, "\n")
cat("Average Conceptual Production Vocabulary:", average_conceptual_produce, "\n")
cat("Range of Conceptual Production Vocabulary:", range_conceptual_produce, "\n")







