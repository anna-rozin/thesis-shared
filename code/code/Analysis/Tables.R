
# Clear previous data
rm(list = ls())

# Load necessary packages
library(knitr)
library(kableExtra)

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)
both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts.csv'), header=TRUE, stringsAsFactors=FALSE)

# Subset the dataframe to include only the columns of interest
subset_df <- both_counts[, c("skill_es_comprehend", "skill_eu_comprehend", 
                             "skill_es_produce", "skill_eu_produce", 
                             "skill_total_comprehend", "skill_concept_comprehend", 
                             "skill_total_produce", "skill_concept_produce")]

# Calculate descriptive statistics using the summary() function
descriptive_stats <- summary(subset_df)

# Convert the summary statistics to a dataframe
CDI_T1_descriptives <- as.data.frame(descriptive_stats)

# Save descriptive statistics as CSV
write.csv(CDI_T1_descriptives, 
          file.path('raw-csv', 'Analysis', 'Descriptives', 'CDI', 'T1', 'CDI_T1_descriptives.csv'),
          row.names=FALSE)

# Print the descriptive statistics dataframe
print(CDI_T1_descriptives)

# Create a formatted table using kable and kableExtra
CDI_T1_table <- kbl(CDI_T1_descriptives, align = c("l", rep("r", ncol(CDI_T1_descriptives)-1))) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = ncol(CDI_T1_descriptives)-1))

# Print the formatted table
print(CDI_T1_table)



######## CREATING CHART 

# Example data (replace with your actual data)
CDI_T1_descriptives <- data.frame(
  Skill_Category = c("Spanish Comprehension", "Basque Comprehension", 
                     "Spanish Production", "Basque Production", 
                     "Total Comprehension", "Concept Comprehension", 
                     "Total Production", "Concept Production"),
  Mean_Score = c(23.33, 97.53, 1.47, 2.33, 120.9, 116.1, 3.8, 3),
  Median_Score = c(10, 84, 0, 0, 100, 96, 0, 0),
  Range = c("[0, 79]", "[12, 314]", "[0, 11]", "[0, 11]", 
            "[18, 330]", "[18, 318]", "[0, 20]", "[0, 16]")
)

# Create the table using kable and kableExtra
CDI_T1_table <- kbl(CDI_T1_descriptives, align = c("l", "c", "c", "c")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = 3)) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle")

# Print the formatted table
print(CDI_T1_table)



#### creating chart updated: 

# Load necessary libraries
library(knitr)
library(kableExtra)

# Example data (replace with your actual data)
CDI_T1_descriptives <- data.frame(
  Skill_Category = c("Spanish Comprehension", "Basque Comprehension", 
                     "Spanish Production", "Basque Production", 
                     "Total Comprehension", "Concept Comprehension", 
                     "Total Production", "Concept Production"),
  Mean_Score = c(20.78, 63.65, 1.696, 2.435, 84.43, 79.74, 4.13, 3.391),
  Median_Score = c(7, 33, 0, 0, 65, 63, 0, 0),
  Range = c("[0.00, 183.00]", "[4.00, 215.00]", "[0.000, 11.000]", "[0.000, 11.000]", 
            "[4.00, 350.00]", "[4.00, 333.00]", "[0.00, 22.00]", "[0.000, 18.000]")
)

# Create the table using kable and kableExtra
CDI_T1_table <- kbl(CDI_T1_descriptives, align = c("l", "c", "c", "c")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = 3)) %>%
  collapse_rows(columns = 1, valign = "middle")

# Print the formatted table
print(CDI_T1_table)




########### LENA 
##### lena descriptives 

# Example data for LENA variables (replace with your actual values)
lena_descriptives <- data.frame(
  Skill_Category = c("CT_COUNT_Avg", "CV_COUNT_Avg", "AWC_COUNT_Avg"),
  Mean_Score = c(33.24, 98.32, 1915),
  Median_Score = c(31.86, 100.62, 1780),
  Range = c("[18.40, 55.11]", "[53.73, 184.52]", "[1244, 3162]")
)

library(knitr)
library(kableExtra)

# Create the table using kable and kableExtra
lena_descriptives_table <- kbl(lena_descriptives, align = c("l", "c", "c", "c")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = 3)) %>%
  column_spec(1, bold = TRUE) %>%
  collapse_rows(columns = 1, valign = "middle")

# Print the formatted table
print(lena_descriptives_table)



#### Vocab Table

# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Data for target words
target_words <- data.frame(
  Type = rep("Target", 16),
  Spanish = c("puerto", "boca", "silla", "mano", "perro", "pan", "caballo", "arbol", "pollo", "barco", "pelota", "leche", "vaca", "gorra", "flor", "llave"),
  Basque = c("portua", "ahoa", "aulkia", "eskua", "txakurra", "ogia", "zaldia", "zuhaitza", "oilaskoa", "itsasontzia", "pilota", "esnea", "behia", "txapela", "lorea", "giltza"),
  `English Translation` = c("door", "mouth", "chair", "hand", "dog", "bread", "horse", "tree", "chicken", "boat", "ball", "milk", "cow", "hat", "flower", "key")
)

# Data for unrelated words
unrelated_words <- data.frame(
  Type = rep("Unrelated", 8),
  Spanish = c("esponja", "muñeca", "coche", "babero", "cepillo", "bañera", "basura", "cuchara"),
  Basque = c("esponja", "panpina", "kotxea", "baberoa", "zepiloa", "bainera", "zakarrontzia", "koilara"),
  `English Translation` = c("sponge", "doll", "car", "bib", "brush", "bathtub", "trash", "spoon")
)

# Create separate tables
table_target <- tableGrob(target_words, rows = NULL)
table_unrelated <- tableGrob(unrelated_words, rows = NULL)

# Title for each table
title_target <- textGrob("Target Words", gp = gpar(fontsize = 20, fontface = "bold"))
title_unrelated <- textGrob("Unrelated Words", gp = gpar(fontsize = 20, fontface = "bold"))

# Arrange title and tables
table_target_with_title <- arrangeGrob(title_target, table_target, nrow = 2, heights = c(0.1, 0.9))
table_unrelated_with_title <- arrangeGrob(title_unrelated, table_unrelated, nrow = 2, heights = c(0.1, 0.9))

# Save tables to files
ggsave("target_words.png", table_target_with_title, width = 10, height = 6)
ggsave("unrelated_words.png", table_unrelated_with_title, width = 10, height = 6)



### T2 CDI
# Clear previous data
rm(list = ls())

# Load necessary packages
library(knitr)
library(kableExtra)

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'), header=TRUE, stringsAsFactors=FALSE)
both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts_T2.csv'), header=TRUE, stringsAsFactors=FALSE)

# Subset the dataframe to include only the columns of interest
subset_df <- both_counts[, c("skill_es_comprehend", "skill_eu_comprehend", 
                             "skill_es_produce", "skill_eu_produce", 
                             "skill_total_comprehend", "skill_concept_comprehend", 
                             "skill_total_produce", "skill_concept_produce")]

# Calculate descriptive statistics using the summary() function
descriptive_stats <- summary(subset_df)

# Convert the summary statistics to a dataframe
CDI_T2_descriptives <- as.data.frame(descriptive_stats)

# Save descriptive statistics as CSV
write.csv(CDI_T2_descriptives, 
          file.path('raw-csv', 'Analysis', 'Descriptives', 'CDI', 'T2', 'CDI_T2_descriptives.csv'),
          row.names=FALSE)

# Print the descriptive statistics dataframe
print(CDI_T2_descriptives)

# Create a formatted table using kable and kableExtra
CDI_T2_table <- kbl(CDI_T2_descriptives, align = c("l", rep("r", ncol(CDI_T2_descriptives)-1))) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = ncol(CDI_T2_descriptives)-1))

# Print the formatted table
print(CDI_T2_table)


# Load necessary libraries
library(knitr)
library(kableExtra)

# Example data for the new Skill Category statistics
new_skill_category_descriptives <- data.frame(
  Skill_Category = c("Spanish Comprehension", "Basque Comprehension", 
                     "Spanish Production", "Basque Production", 
                     "Total Comprehension", "Concept Comprehension", 
                     "Total Production", "Concept Production"),
  Mean_Score = c(56.53, 116.4, 4.133, 7.267, 172.9, 163.5, 11.4, 9.8),
  Median_Score = c(28, 86, 1, 4, 156, 150, 4, 4),
  Range = c("[0.00, 152.00]", "[19.0, 290.0]", "[0.000, 17.000]", "[0.000, 25.000]", 
            "[23.0, 328.0]", "[21.0, 317.0]", "[0.0, 34.0]", "[0.0, 30.0]")
)

# Create the table using kable and kableExtra
new_skill_category_table <- kbl(new_skill_category_descriptives, align = c("l", "c", "c", "c")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Skill Category" = 1, "Statistics" = 3)) %>%
  collapse_rows(columns = 1, valign = "middle")

# Print the formatted table
print(new_skill_category_table)
