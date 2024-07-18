# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load necessary libraries
library(ggplot2)
library(dplyr)

############### Creating a new table ########################

# Extract the unique participant IDs
unique_ids <- unique(summary_dat$ID_lab)

# Initialize the new data frame with the unique participant IDs
CDI_AVG <- data.frame(ID_lab = unique_ids)

# Sum the binary values for each skill
CDI_AVG$skill_es_comprehend <- sapply(unique_ids, function(id) {
  sum(summary_dat$skill_es_comprehend[summary_dat$ID_lab == id], na.rm = TRUE)
})

CDI_AVG$skill_eu_comprehend <- sapply(unique_ids, function(id) {
  sum(summary_dat$skill_eu_comprehend[summary_dat$ID_lab == id], na.rm = TRUE)
})

CDI_AVG$skill_es_produce <- sapply(unique_ids, function(id) {
  sum(summary_dat$skill_es_produce[summary_dat$ID_lab == id], na.rm = TRUE)
})

CDI_AVG$skill_eu_produce <- sapply(unique_ids, function(id) {
  sum(summary_dat$skill_eu_produce[summary_dat$ID_lab == id], na.rm = TRUE)
})

# Calculate total comprehension
CDI_AVG$skill_total_comprehend <- CDI_AVG$skill_es_comprehend + CDI_AVG$skill_eu_comprehend

# Conceptual comprehension: count unique concepts known in either language
CDI_AVG$skill_concept_comprehend <- sapply(unique_ids, function(id) {
  # Extract comprehension data for the participant
  eu_comprehend <- summary_dat$skill_eu_comprehend[summary_dat$ID_lab == id]
  es_comprehend <- summary_dat$skill_es_comprehend[summary_dat$ID_lab == id]
  
  # Sum the rows where either eu or es comprehension is 1, considering unique concepts
  sum((eu_comprehend == 1) | (es_comprehend == 1), na.rm = TRUE)
})

# Calculate total production
CDI_AVG$skill_total_produce <- CDI_AVG$skill_es_produce + CDI_AVG$skill_eu_produce

# Conceptual production: count unique concepts produced in either language
CDI_AVG$skill_concept_produce <- sapply(unique_ids, function(id) {
  # Extract production data for the participant
  eu_produce <- summary_dat$skill_eu_produce[summary_dat$ID_lab == id]
  es_produce <- summary_dat$skill_es_produce[summary_dat$ID_lab == id]
  
  # Sum the rows where either eu or es production is 1, considering unique concepts
  sum((eu_produce == 1) | (es_produce == 1), na.rm = TRUE)
})

# Display the first few rows of the new data frame
head(CDI_AVG)

#### adding bil and mono 
mono_bil <- read.csv(file.path('source','CDI', 'Mono_Bil.csv'), header=TRUE, stringsAsFactors=FALSE)

# Merge summary_dat with mono_bil dataframe based on ID_lab
CDI_AVG <- merge(CDI_AVG, mono_bil, by = "ID_lab", all.x = TRUE)


# Remove ACCORDING to df
CDI_AVG <- CDI_AVG[-16, ]




# Define the new order of column names
new_order <- c("ID_lab", "Sex", "Lang.group","L1", "L2", "L1.", "L2.", "skill_es_comprehend", "skill_eu_comprehend", 
               "skill_es_produce", "skill_eu_produce", "skill_total_comprehend", 
               "skill_concept_comprehend", "skill_total_produce", 
               "skill_concept_produce")

# Reorder the columns in the CDI_AVG dataframe
CDI_AVG <- CDI_AVG[, new_order]

colnames(CDI_AVG)[colnames(CDI_AVG) == "Lang.group"] <- "Mono_Bil"
colnames(CDI_AVG)[colnames(CDI_AVG) == "L1."] <- "L1_p"
colnames(CDI_AVG)[colnames(CDI_AVG) == "L2."] <- "L2_p"



# Rename CDI_AVG to cdi_counts
cdi_counts <- CDI_AVG


#save as CSV
write.csv(cdi_counts, 
          file.path('raw-csv', 'Analysis', '9.CDI_participants_T2.csv'),
          row.names=FALSE)



############### 
#CHECKING 


# Check unique participant IDs in summary_dat
unique_ids_summary <- unique(summary_dat$ID_lab)

# Example subsetting condition
participant_data <- summary_dat[summary_dat$ID_lab == "6943", ]

# View structure of summary_dat
str(summary_dat)


# Remove duplicates within each participant group for specified columns
cleaned_summary_dat <- summary_dat %>%
  group_by(ID_lab) %>%
  distinct(skill_eu_comprehend, skill_es_comprehend, .keep_all = TRUE) %>%
  ungroup()

# Count the number of rows for each participant in the cleaned dataset
cleaned_participant_row_counts <- cleaned_summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(cleaned_participant_row_counts)

# View the cleaned dataset
print(cleaned_summary_dat)


# Load necessary library
library(dplyr)

# Group by ID_lab and count the number of rows for each participant
participant_row_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Check for duplicates within each participant group for specified columns
duplicates_within_group <- summary_dat %>%
  group_by(ID_lab) %>%
  filter(duplicated(cbind(skill_eu_comprehend, skill_es_comprehend)))

# View duplicates within each group if any
print(duplicates_within_group)

# Load necessary library
library(dplyr)

# Group by ID_lab and count the number of rows for each participant based on the specified columns
participant_row_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Check for duplicates within each participant group for specified columns
duplicates_within_group <- summary_dat %>%
  group_by(ID_lab) %>%
  mutate(duplicate = duplicated(select(., skill_eu_comprehend, skill_es_comprehend))) %>%
  filter(duplicate == TRUE) %>%
  ungroup()

# View duplicates within each group if any
print(duplicates_within_group)



# Count the number of rows for each participant
participant_row_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Check for duplicates within each participant group
duplicates_within_group <- summary_dat %>%
  group_by(ID_lab) %>%
  mutate(duplicate = duplicated(.)) %>%
  filter(duplicate == TRUE) %>%
  ungroup()

# View duplicates within each group if any
print(duplicates_within_group)



# Check for duplicate rows in the entire dataset
duplicates <- summary_dat %>%
  filter(duplicated(.))

# View duplicates if any
print(duplicates)

# Check for duplicates within each participant group
duplicates_within_group <- summary_dat %>%
  group_by(ID_lab) %>%
  filter(duplicated(.))

# View duplicates within each group if any
print(duplicates_within_group)



# Assuming your dataset is named summary_dat
# Group by ID_lab and then summarize the sum of the measures
summary_result <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(
    total_skill_eu_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    total_skill_es_comprehend = sum(skill_es_comprehend, na.rm = TRUE)
  )

# View the result
print(summary_result)

participant_6937B_rows <- summary_dat %>%
  filter(ID_lab == "6937B") %>%
  nrow()

# Print the number of rows
print(participant_6937B_rows)

# Load necessary library
library(dplyr)

# Group by ID_lab and count the number of rows for each participant
participant_row_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Load necessary library
library(dplyr)


# View the structure of the dataset to ensure there are no issues
str(summary_dat)

# Group by ID_lab and count the number of rows for each participant
participant_row_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Optionally, check for duplicates if you suspect data issues
duplicates <- summary_dat %>%
  group_by(ID_lab) %>%
  filter(duplicated(.))

# View duplicates if any
print(duplicates)



# Filter for participant 7135 and skill_es_comprehend
participant_7135_skill_eu_comprehend <- summary_dat %>%
  filter(ID_lab == "7135")

# Tally of 1 values for skill_es_comprehend
tally_skill_eu_comprehend <- sum(participant_7135_skill_eu_comprehend$skill_eu_comprehend, na.rm = TRUE)

# Print the tally
print(tally_skill_eu_comprehend)

# Filter for participant 7135 and skill_es_comprehend
participant_7135_skill_es_comprehend <- summary_dat %>%
  filter(ID_lab == "7135") %>%
  select(ID_lab, skill_es_comprehend)

# Print the result
print(participant_7135_skill_es_comprehend)

# Filter for participant 7135 and skill_es_comprehend
participant_7135_skill_es_comprehend <- summary_dat %>%
  filter(ID_lab == "7135") %>%
  select(ID_lab, total_ones_es_comprehend)

# Print the result
print(participant_7135_skill_es_comprehend)


# Filter for participant 7135
participant_7135 <- summary_dat %>%
  filter(ID_lab == "7135")

# Print the result
print(participant_7135)


# Filter for participant 7135
summary_dat <- skill_eu_comprehend %>%
  filter(ID_lab == "7135")

# Print the result
print(participant_7135)

library(dplyr)

result <- summary_dat %>%
  group_by(ID_lab) %>%
  summarize(total_ones = sum(skill_eu_comprehend == 1, na.rm = TRUE))

unique(merged_CDI_LENA$skill_eu_comprehend)

unique_values <- unique(merged_CDI_LENA$skill_eu_comprehend)
print(unique_values)

table(merged_CDI_LENA$skills_eu_comprehend)

print(result)

# Recompute the tally of 1's and 0's for each participant
result <- merged_CDI_LENA %>%
  group_by(ID_lab) %>%
  summarize(
    total_ones = sum(skill_eu_comprehend == 1, na.rm = TRUE),
    total_zeros = sum(skill_eu_comprehend == 0, na.rm = TRUE)
  )

# View the result
print(result)

print(merged_CDI_LENA$skill_eu_comprehend)

# Filter the data for participant "6937B"
participant_6937B <- merged_CDI_LENA %>%
  filter(ID_lab == "6937B")

# Check the distribution of values in the 'skills_eu_comprehend' column for participant "6937B"
table(participant_6937B$skill_eu_comprehend)


print(participant_6937B$skill_eu_comprehend)

# Recompute the tally of 1's and 0's for each participant
result <- merged_CDI_LENA %>%
  group_by(ID_lab) %>%
  summarize(
    total_ones = sum(skill_eu_comprehend == 1, na.rm = TRUE),
    total_zeros = sum(skill_eu_comprehend == 0, na.rm = TRUE),
    total_nas = sum(is.na(skill_eu_comprehend))
  )

# View the result
print(result)

# View the result
print(result)



##################### might delete everything below, saving in case for now
#################
# Calculate total average and range
T_avg <- mean(CDI_AVG$column_name)
T_range <- diff(range(CDI_AVG$column_name))

# Calculate monolingual average and range
mono_avg <- mean(CDI_AVG$column_name[CDI_AVG$bi_mono == "mono"])
mono_range <- diff(range(CDI_AVG$column_name[CDI_AVG$bi_mono == "mono"]))

# Calculate bilingual average and range
bi_avg <- mean(CDI_AVG$column_name[CDI_AVG$bi_mono == "bi"])
bi_range <- diff(range(CDI_AVG$column_name[CDI_AVG$bi_mono == "bi"]))

# Create data frame for new rows
new_rows <- data.frame(
  ID_lab = c("T_avg", "T_range", "mono_avg", "mono_range", "bi_avg", "bi_range"),
  column_name = c(T_avg, T_range, mono_avg, mono_range, bi_avg, bi_range)
)

# Add new rows to CDI_AVG
CDI_AVG <- rbind(CDI_AVG, new_rows)

# Print the updated CDI_AVG data frame
print(CDI_AVG)




#############################
##### EU_COMPREHENSION AVG AND RANGE 
# Aggregate data to get total known words for EU comprehension by participant
total_words_known_eu_comprehend <- aggregate(skill_eu_comprehend ~ ID_lab, summary_dat, sum)

# Display the aggregated data
print(total_words_known_eu_comprehend)

# Calculate the average number of words known for EU comprehension
average_words_known_eu_comprehend <- mean(total_words_known_eu_comprehend$skill_eu_comprehend, na.rm = TRUE)

# Calculate the range of the total number of words known for EU comprehension
range_words_known_eu_comprehend <- diff(range(total_words_known_eu_comprehend$skill_eu_comprehend, na.rm = TRUE))

# Display the results
cat("Average Words Known for EU Comprehension:", average_words_known_eu_comprehend, "\n")
cat("Range of Words Known for EU Comprehension:", range_words_known_eu_comprehend, "\n")

################ avg and ranges 
# Calculate averages and ranges for each skill

# skill_es_comprehend
average_skill_es_comprehend <- mean(CDI_AVG$skill_es_comprehend, na.rm = TRUE)
range_skill_es_comprehend <- diff(range(CDI_AVG$skill_es_comprehend, na.rm = TRUE))

# skill_eu_comprehend
average_skill_eu_comprehend <- mean(CDI_AVG$skill_eu_comprehend, na.rm = TRUE)
range_skill_eu_comprehend <- diff(range(CDI_AVG$skill_eu_comprehend, na.rm = TRUE))

# skill_es_produce
average_skill_es_produce <- mean(CDI_AVG$skill_es_produce, na.rm = TRUE)
range_skill_es_produce <- diff(range(CDI_AVG$skill_es_produce, na.rm = TRUE))

# skill_eu_produce
average_skill_eu_produce <- mean(CDI_AVG$skill_eu_produce, na.rm = TRUE)
range_skill_eu_produce <- diff(range(CDI_AVG$skill_eu_produce, na.rm = TRUE))

# skill_total_comprehend
average_skill_total_comprehend <- mean(CDI_AVG$skill_total_comprehend, na.rm = TRUE)
range_skill_total_comprehend <- diff(range(CDI_AVG$skill_total_comprehend, na.rm = TRUE))

# skill_concept_comprehend
average_skill_concept_comprehend <- mean(CDI_AVG$skill_concept_comprehend, na.rm = TRUE)
range_skill_concept_comprehend <- diff(range(CDI_AVG$skill_concept_comprehend, na.rm = TRUE))

# Print the results
cat("Average skill_es_comprehend:", average_skill_es_comprehend, "\n")
cat("Range of skill_es_comprehend:", range_skill_es_comprehend, "\n")

cat("Average skill_eu_comprehend:", average_skill_eu_comprehend, "\n")
cat("Range of skill_eu_comprehend:", range_skill_eu_comprehend, "\n")

cat("Average skill_es_produce:", average_skill_es_produce, "\n")
cat("Range of skill_es_produce:", range_skill_es_produce, "\n")

cat("Average skill_eu_produce:", average_skill_eu_produce, "\n")
cat("Range of skill_eu_produce:", range_skill_eu_produce, "\n")

cat("Average skill_total_comprehend:", average_skill_total_comprehend, "\n")
cat("Range of skill_total_comprehend:", range_skill_total_comprehend, "\n")

cat("Average skill_concept_comprehend:", average_skill_concept_comprehend, "\n")
cat("Range of skill_concept_comprehend:", range_skill_concept_comprehend, "\n")

################# adding them 
# Calculate averages and ranges for each skill




##### ES_COMPREHENSION AVG AND RANGE 
# Aggregate data to get total known words for ES comprehension by participant
total_words_known_es_comprehend <- aggregate(skill_es_comprehend ~ ID_lab, summary_dat, sum)

# Display the aggregated data
print(total_words_known_es_comprehend)

# Calculate the average number of words known for ES comprehension
average_words_known_es_comprehend <- mean(total_words_known_es_comprehend$skill_es_comprehend, na.rm = TRUE)

# Calculate the range of the total number of words known for ES comprehension
range_words_known_es_comprehend <- diff(range(total_words_known_es_comprehend$skill_es_comprehend, na.rm = TRUE))

# Display the results
cat("Average Words Known for ES Comprehension:", average_words_known_es_comprehend, "\n")
cat("Range of Words Known for ES Comprehension:", range_words_known_es_comprehend, "\n")

##### EU_PRODUCE AVG AND RANGE 
# Aggregate data to get total known words for EU production by participant
total_words_known_eu_produce <- aggregate(skill_eu_produce ~ ID_lab, summary_dat, sum)

# Display the aggregated data
print(total_words_known_eu_produce)

# Calculate the average number of words known for EU production
average_words_known_eu_produce <- mean(total_words_known_eu_produce$skill_eu_produce, na.rm = TRUE)

# Calculate the range of the total number of words known for EU production
range_words_known_eu_produce <- diff(range(total_words_known_eu_produce$skill_eu_produce, na.rm = TRUE))

# Display the results
cat("Average Words Known for EU Production:", average_words_known_eu_produce, "\n")
cat("Range of Words Known for EU Production:", range_words_known_eu_produce, "\n")

##### ES_PRODUCE AVG AND RANGE 
# Aggregate data to get total known words for EU production by participant
total_words_known_es_produce <- aggregate(skill_es_produce ~ ID_lab, summary_dat, sum)

# Display the aggregated data
print(total_words_known_es_produce)

# Calculate the average number of words known for EU production
average_words_known_es_produce <- mean(total_words_known_es_produce$skill_es_produce, na.rm = TRUE)

# Calculate the range of the total number of words known for EU production
range_words_known_es_produce <- diff(range(total_words_known_es_produce$skill_es_produce, na.rm = TRUE))

# Display the results
cat("Average Words Known for ES Production:", average_words_known_es_produce, "\n")
cat("Range of Words Known for ES Production:", range_words_known_es_produce, "\n")


### BILINGUALS comprehension

# Filter out participants who have non-NA responses for both EU and ES comprehension
bilingual_participants <- summary_dat %>%
  filter(!is.na(skill_eu_comprehend) & !is.na(skill_es_comprehend))

# Aggregate data to get total known words for both EU and ES comprehension by participant
total_words_known_bilingual_comprehend <- bilingual_participants %>%
  group_by(ID_lab) %>%
  summarize(
    total_skill_eu_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    total_skill_es_comprehend = sum(skill_es_comprehend, na.rm = TRUE)
  )

# Calculate the total known words for both EU and ES comprehension
total_words_known_bilingual_comprehend$total_comprehend <- total_words_known_bilingual_comprehend$total_skill_eu_comprehend + total_words_known_bilingual_comprehend$total_skill_es_comprehend

# Display the aggregated data
print(total_words_known_bilingual_comprehend)

# Calculate the average number of words known for both EU and ES comprehension
average_words_known_bilingual_comprehend <- mean(total_words_known_bilingual_comprehend$total_comprehend, na.rm = TRUE)

# Calculate the range of the total number of words known for both EU and ES comprehension
range_words_known_bilingual_comprehend <- diff(range(total_words_known_bilingual_comprehend$total_comprehend, na.rm = TRUE))

# Display the results
cat("Average Words Known for Bilingual Comprehension (EU + ES):", average_words_known_bilingual_comprehend, "\n")
cat("Range of Words Known for Bilingual Comprehension (EU + ES):", range_words_known_bilingual_comprehend, "\n")

### Bilingual production 

# Filter out participants who have non-NA responses for both EU and ES production
bilingual_participants_production <- summary_dat %>%
  filter(!is.na(skill_eu_produce) & !is.na(skill_es_produce))

# Aggregate data to get total known words for both EU and ES production by participant
total_words_known_bilingual_production <- bilingual_participants_production %>%
  group_by(ID_lab) %>%
  summarize(
    total_skill_eu_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_skill_es_produce = sum(skill_es_produce, na.rm = TRUE)
  )

# Calculate the total known words for both EU and ES production
total_words_known_bilingual_production$total_produce <- total_words_known_bilingual_production$total_skill_eu_produce + total_words_known_bilingual_production$total_skill_es_produce

# Display the aggregated data
print(total_words_known_bilingual_production)

# Calculate the average number of words known for both EU and ES production
average_words_known_bilingual_produce <- mean(total_words_known_bilingual_production$total_produce, na.rm = TRUE)

# Calculate the range of the total number of words known for both EU and ES production
range_words_known_bilingual_produce <- diff(range(total_words_known_bilingual_production$total_produce, na.rm = TRUE))

# Display the results
cat("Average Words Known for Bilingual Production (EU + ES):", average_words_known_bilingual_produce, "\n")
cat("Range of Words Known for Bilingual Production (EU + ES):", range_words_known_bilingual_produce, "\n")





