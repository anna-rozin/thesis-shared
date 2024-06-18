
# Clear previous data
rm(list = ls())

# Load data
lena_summary <- read.csv(file.path('raw-csv','Summary','1.lena_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
CDI_summary <- read.csv(file.path('raw-csv','Summary','0.merged_CDI_raw.csv'), header=TRUE, stringsAsFactors=FALSE)

# Perform the merge based on the common identifier, keeping all columns
merged_CDI_LENA <- merge(CDI_summary, lena_summary, by.x = "ID_lab", by.y = "ExternalReferenceID", all.x = TRUE, all.y = TRUE)


# Check the number of rows for each participant after the merge
merged_row_counts <- merged_CDI_LENA%>%
  group_by(ID_lab) %>%
  summarise(n = n())

# View the result
print(merged_row_counts)

###########

#save as CSV
write.csv(merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '7.merged_CDIs_summary.csv'),
          row.names=FALSE)





### Set working directory and load data

# Clear previous data
rm(list = ls())

# Load data
lena_summary <- read.csv(file.path('raw-csv','Summary','1.lena_Summary.csv'), header=TRUE, stringsAsFactors=FALSE)
CDI_summary <- read.csv(file.path('raw-csv','Summary','0.merged_CDI_raw.csv'), header=TRUE, stringsAsFactors=FALSE)

# Perform the merge based on the common identifier, keeping all columns
merged_CDI_LENA <- merge(CDI_summary, lena_summary, by.x = "ID_lab", by.y = "ExternalReferenceID", all.x = TRUE, all.y = TRUE)


# Check the number of rows for each participant after the merge
merged_row_counts <- merged_CDI_LENA%>%
  group_by(ID_lab) %>%
  summarise(n = n())

# View the result
print(merged_row_counts)

###########

#save as CSV
write.csv(merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '7.merged_CDIs_summary.csv'),
          row.names=FALSE)


################ CHECKING 

library(dplyr)

# Remove duplicates within each group (ID_lab) based on item_code and part_code
cleaned_merged_CDI_LENA <- merged_CDI_LENA %>%
  group_by(ID_lab) %>%
  distinct(item_code, part_code, .keep_all = TRUE)

# Count the number of rows for each participant
participant_row_counts <- CDI_summary%>%
  count(ID_lab)

# Print the result
print(participant_row_counts)



###########

#save as CSV
write.csv(cleaned_merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '7.merged_CDIs_summary.csv'),
          row.names=FALSE)






################ CHECKING 

# Remove duplicate rows based on item_code and part_code
cleaned_merged_CDI_LENA <- merged_CDI_LENA[!duplicated(merged_CDI_LENA[, c("item_code", "part_code")]), ]

# View the result
print(cleaned_merged_CDI_LENA)


# Count the total number of duplicate rows
total_duplicates <- sum(duplicated(merged_CDI_LENA))

# Print the total count of duplicates
print(total_duplicates)

# Print the first few rows of duplicate rows
head(duplicate_rows)




participant_row_counts <- merged_CDI_LENA %>%
  group_by(ID_lab) %>%
  summarise(row_count = n())

# View the result
print(participant_row_counts)

