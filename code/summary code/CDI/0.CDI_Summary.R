# Clear previous data
rm(list = ls())

library(dplyr)

B_CDI <- read.csv(file.path('raw-csv', '5.B_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
B2_CDI <- read.csv(file.path('raw-csv','5.B2_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
S_CDI<- read.csv(file.path('raw-csv','5.S_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)
S2_CDI <- read.csv(file.path('raw-csv','5.S2_CDI_FINALRAW.csv'), header=TRUE, stringsAsFactors=FALSE)

#Merge all parts of summary CDI
merged_CDI_raw <- rbind(B_CDI, B2_CDI, S_CDI, S2_CDI)

############# attempting 
skill_eu_comprehend_summary <- merged_CDI_raw %>%
  group_by(ID_lab) %>%
  summarise(skill_eu_comprehend_sum = sum(skill_eu_comprehend, na.rm = TRUE))


#save as summary csv
write.csv(merged_CDI_raw,
          file.path('raw-csv', 'Summary', '0.merged_CDI_raw.csv'),
          row.names=FALSE)





#######checking 

# Load necessary library
library(dplyr)

# Filter out rows with NA or empty response, then remove duplicates based on BQ_word for each participant
merged_CDI_raw_unique <- merged_CDI_raw %>%
  filter(!is.na(response) & response != "") %>%
  group_by(ID_lab, BQ_word) %>%
  slice(1) %>%  # Keep the first occurrence within each group of ID_lab and BQ_word
  ungroup()

# View the result
print(merged_CDI_raw_unique)

# Count the number of rows per participant after removing duplicates
row_counts_unique <- merged_CDI_raw_unique %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(row_counts_unique)






# Load necessary library
library(dplyr)

# Check for duplicate BQ_words per ID_lab, ignoring NA and empty strings
duplicates <- merged_CDI_raw %>%
  filter(BQ_word != "" & !is.na(BQ_word)) %>%
  group_by(ID_lab, BQ_word) %>%
  filter(n() > 1) %>%
  arrange(ID_lab, BQ_word)

# View the result
print(duplicates)


# View a sample of the duplicates
print(head(duplicates, 20))

# Count the number of rows per participant
row_counts <- merged_CDI_raw %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(row_counts)


###### DELETE
######### checking columns 
# Count the number of rows for each participant
participant_row_counts <- merged_CDI_raw %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)


# Remove duplicates within each participant group based on specified columns
merged_CDI_raw <- merged_CDI_raw %>%
  group_by(ID_lab) %>%
  distinct(item_code, part_code, .keep_all = TRUE) %>%
  ungroup()

# Count the number of rows for each participant in the cleaned dataset
participant_row_counts <- merged_CDI_raw %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

####__________
#Creating a summary df that will be merged with lena

# Define the columns to be removed
columns_to_remove <- c("part_code", "part", "item_code", "response")

# Create a new dataframe with the specified columns removed
merged_CDI_summary<- merged_CDI_raw[, !(names(merged_CDI_raw) %in% columns_to_remove)]


#save as summary csv
write.csv(merged_CDI_raw,
          file.path('raw-csv', 'Summary', 'merged_CDI_raw.csv'),
          row.names=FALSE)

