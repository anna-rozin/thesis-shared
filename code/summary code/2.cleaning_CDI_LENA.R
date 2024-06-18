# Clear previous data
rm(list = ls())

# Load data
merged_CDI_LENA <- read.csv(file.path('raw-csv', 'Summary', '7.merged_CDIs_summary.csv'), header=TRUE, stringsAsFactors=FALSE)

# Renaming some columns
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "Recording_Gender"] <- "gender"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "Recording_DOB"] <- "DOB"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "RecordingDate"] <- "testing_lena"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "date"] <- "testing_CDI"
names(merged_CDI_LENA)[names(merged_CDI_LENA) == "lang"] <- "lang_response"


####### Adding age of CDI completion 

# Convert 'testing_CDI' to Date object
merged_CDI_LENA$testing_CDI <- as.Date(merged_CDI_LENA$testing_CDI)

# Convert 'DOB' to Date object with correct format
merged_CDI_LENA$DOB <- as.Date(merged_CDI_LENA$DOB, format = "%m/%d/%Y")

# Calculate age in days
age_days <- as.numeric(difftime(merged_CDI_LENA$testing_CDI, merged_CDI_LENA$DOB, units = "days"))

# Convert age in days to months
merged_CDI_LENA$age_months <- age_days / 30.4375  # Average number of days in a month

# Rename the new age column to 'age_CDI'
colnames(merged_CDI_LENA)[which(names(merged_CDI_LENA) == "age_months")] <- "age_CDI"

####### Adding age for lena testing


# Convert 'testing_lena' to Date object with correct format
merged_CDI_LENA$testing_lena <- as.Date(merged_CDI_LENA$testing_lena, format = "%Y-%m-%d")
# Convert 'DOB' to Date object with correct format
merged_CDI_LENA$DOB <- as.Date(merged_CDI_LENA$DOB, format = "%Y-%m-%d")

# Calculate age in days for LENA
age_days_lena <- as.numeric(difftime(merged_CDI_LENA$testing_lena, merged_CDI_LENA$DOB, units = "days"))

# Convert age in days to months for LENA
merged_CDI_LENA$age_months_lena <- age_days_lena / 30.4375  # Average number of days in a month

# Rename the new age column to 'age_lena'
colnames(merged_CDI_LENA)[which(names(merged_CDI_LENA) == "age_months_lena")] <- "age_lena"



##### fixing the order of everything 

# Print all column names
print(names(merged_CDI_LENA))


# Specify the new column order
new_order <- c("ID_lab", "ID", "gender", "DOB", "testing_CDI", "age_CDI","token", "completed", "language", 
               "lang_response", "skill_es_comprehend", "skill_es_produce", "skill_eu_comprehend", "skill_eu_produce", 
               "testing_lena", "age_lena", "CT_COUNT_Avg", "AWC_COUNT_Avg", "CV_COUNT_Avg")

# Reorder columns
merged_CDI_LENA <- merged_CDI_LENA[, new_order]



# removing all time points except for time point 1 from token column 
# Filter the data frame to keep only rows where "token" equals 1
merged_CDI_LENA <- merged_CDI_LENA[merged_CDI_LENA$token == "1", , drop = FALSE]


#save as CSV
write.csv(merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'),
          row.names=FALSE)


########### TIME POINT 2

# removing all time points except for time point 1 from token column 
# Filter the data frame to keep only rows where "token" equals 2
merged_CDI_LENA <- merged_CDI_LENA[merged_CDI_LENA$token == "2", , drop = FALSE]


#save as CSV
write.csv(merged_CDI_LENA, 
          file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'),
          row.names=FALSE)



