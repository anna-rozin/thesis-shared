# Clear previous data
rm(list = ls())

#Load data
final_df <- read.csv(file.path('raw-csv', '3.S2_CDI_cleaned.responses.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)

#-------------------------------------------------#
### FIXING DUPLICATE ROWS


# Remove duplicates within each participant group based on specified columns
final_df <- final_df %>%
  group_by(ID_lab) %>%
  distinct(item, part_code, .keep_all = TRUE) %>%
  ungroup()

# Count the number of rows for each participant in the cleaned dataset
participant_row_counts <- final_df %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)



################

# Define function to convert responses to binary skills for "skill_es_comprehend"
convert_response_to_skill_es_comprehend <- function(response) {
  ifelse(response %in% c("Comprende", "Comprende / Ulertzen du", "Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_es_produce"
convert_response_to_skill_es_produce <- function(response) {
  ifelse(response %in% c("Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_eu_comprehend"
convert_response_to_skill_eu_comprehend <- function(response) {
  ifelse(response %in% c("Ulertzen du", "Comprende / Ulertzen du", "Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_eu_produce"
convert_response_to_skill_eu_produce <- function(response) {
  ifelse(response %in% c("Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Create binary skill columns based on "response" column
final_df$skill_es_comprehend <- convert_response_to_skill_es_comprehend(final_df$response)
final_df$skill_es_produce <- convert_response_to_skill_es_produce(final_df$response)
final_df$skill_eu_comprehend <- convert_response_to_skill_eu_comprehend(final_df$response)
final_df$skill_eu_produce <- convert_response_to_skill_eu_produce(final_df$response)

# Ensure if produce is 1, comprehend is also 1
final_df$skill_es_comprehend <- ifelse(final_df$skill_es_produce == 1, 1, final_df$skill_es_comprehend)
final_df$skill_eu_comprehend <- ifelse(final_df$skill_eu_produce == 1, 1, final_df$skill_eu_comprehend)




##__________________________
# survey completion, 1 means it is completed

# Apply the transformation: 15 becomes 1, all others become 0
final_df$completed <- ifelse(final_df$completed == 15, 1, 0)


#save as skills csv
write.csv(final_df, 
          file.path('raw-csv', '4.S2_CDI_skills.csv'),
          row.names=FALSE)



########### CHECKING 

# Load necessary library
library(dplyr)

# Count the number of rows for each participant
participant_row_counts <- final_df %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

# Load necessary library
library(dplyr)

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

# Load necessary library
library(dplyr)

# Load necessary library
library(dplyr)

# Remove duplicates within each participant group based on specified columns
cleaned_final_df <- final_df %>%
  group_by(ID_lab) %>%
  distinct(item, part_code, .keep_all = TRUE) %>%
  ungroup()

# Count the number of rows for each participant in the cleaned dataset
cleaned_participant_row_counts <- cleaned_final_df %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(cleaned_participant_row_counts)



################ DELETE BELOW 

###### making the responses into binaries na, 0, and 1
#and creating 4 columns for the responses 

# Define function to convert responses to binary skills for "skill_es_comprehend"
convert_response_to_skill_es_comprehend <- function(response) {
  ifelse(response %in% c("Comprende", "Comprende / Ulertzen du", "Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, NA)
}

# Define function to convert responses to binary skills for "skill_es_produce"
convert_response_to_skill_es_produce <- function(response) {
  ifelse(response %in% c("Comprende y dice", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Define function to convert responses to binary skills for "skill_eu_comprehend"
convert_response_to_skill_eu_comprehend <- function(response) {
  ifelse(response %in% c("Ulertzen du", "Comprende / Ulertzen du", "Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, NA)
}

# Define function to convert responses to binary skills for "skill_eu_produce"
convert_response_to_skill_eu_produce <- function(response) {
  ifelse(response %in% c("Ulertu eta esaten du", "Comprende y dice / Ulertu eta esaten du"), 1, 0)
}

# Create binary skill columns based on "response" column
final_df$skill_es_comprehend <- ifelse(is.na(final_df$response) | final_df$response == "", NA, convert_response_to_skill_es_comprehend(final_df$response))
final_df$skill_es_produce <- ifelse(is.na(final_df$response) | final_df$response == "", NA, convert_response_to_skill_es_produce(final_df$response))
final_df$skill_eu_comprehend <- ifelse(is.na(final_df$response) | final_df$response == "", NA, convert_response_to_skill_eu_comprehend(final_df$response))
final_df$skill_eu_produce <- ifelse(is.na(final_df$response) | final_df$response == "", NA, convert_response_to_skill_eu_produce(final_df$response))

# Ensure if produce is 1, comprehend is also 1
final_df$skill_es_comprehend <- ifelse(final_df$skill_es_produce == 1, 1, final_df$skill_es_comprehend)
final_df$skill_eu_comprehend <- ifelse(final_df$skill_eu_produce == 1, 1, final_df$skill_eu_comprehend)

# Ensure if comprehend is NA, produce is also NA
final_df$skill_es_produce <- ifelse(is.na(final_df$skill_es_comprehend), NA, final_df$skill_es_produce)
final_df$skill_eu_produce <- ifelse(is.na(final_df$skill_eu_comprehend), NA, final_df$skill_eu_produce)

