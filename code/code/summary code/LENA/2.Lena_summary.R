# load data 
lena <- read.csv(file.path('raw-csv', 'Summary', 'lena_Summary.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)


#################### I am getting rid of all time points except for the 1st one 

# Convert RecordingDate to date format
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Assuming your dataframe is called lena_df
lena <- lena %>%
  filter(!(ExternalReferenceID == '6937B' & RecordingDate == '2023-10-03'))

library(dplyr)


# Step 1: Find the first two distinct dates for each participant
first_two_dates <- lena %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  distinct(RecordingDate, .keep_all = TRUE) %>%
  slice_head(n = 2) %>%
  select(ExternalReferenceID, RecordingDate)

# Step 2: Filter the original data to keep all rows from the first two dates
lena_first_two_dates <- lena %>%
  semi_join(first_two_dates, by = c("ExternalReferenceID", "RecordingDate")) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate, 
         StartTime, EndTime, CT_COUNT, AWC_COUNT, CV_COUNT)


############ Creating Threshold Column 

# Step 3: Create the "threshold" column based on the "AWC_COUNT" column
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(threshold = ifelse(AWC_COUNT <= 10, 0, ifelse(AWC_COUNT >= 11, 1, NA)))


#### creating a new data frame, to calculate average/ hour. Excluding hours in Threshold with 0

# Step 4: Summarize the data per participant
summary_df <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  summarize(
    Threshold_Sum = sum(threshold, na.rm = TRUE),
    CT_COUNT_Sum = sum(CT_COUNT[threshold == 1], na.rm = TRUE),
    AWC_COUNT_Sum = sum(AWC_COUNT[threshold == 1], na.rm = TRUE),
    CV_COUNT_Sum = sum(CV_COUNT[threshold == 1], na.rm = TRUE)
  ) %>%
  mutate(
    CT_COUNT_Avg = CT_COUNT_Sum / Threshold_Sum,
    AWC_COUNT_Avg = AWC_COUNT_Sum / Threshold_Sum,
    CV_COUNT_Avg = CV_COUNT_Sum / Threshold_Sum
  )


##### creating new df with only avgs and needed info 

# Step 5: Get the earliest recording date per participant
earliest_dates <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  slice_head(n = 1) %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate) %>%
  ungroup()

# Step 6: Merge the summary with the demographic and recording information
final_df <- earliest_dates %>%
  left_join(summary_df, by = "ExternalReferenceID") %>%
  select(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate,
         CT_COUNT_Avg, AWC_COUNT_Avg, CV_COUNT_Avg)



#save as a summary csv 
write.csv(final_df, 
          file.path('raw-csv', 'Summary', '1.lena_Summary.csv'),
          row.names=FALSE)



######### MIN/ MAX
library(dplyr)

# Step 1: Calculate Duration_Hours from StartTime and EndTime
lena_first_two_dates <- lena_first_two_dates %>%
  mutate(
    StartTime = as.POSIXct(StartTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Europe/Madrid"),
    EndTime = as.POSIXct(EndTime, format = "%m/%d/%y %I:%M:%S %p", tz = "Europe/Madrid"),
    Duration_Hours = as.numeric(difftime(EndTime, StartTime, units = "hours"))
  )

# Step 2: Group by ExternalReferenceID and summarize the durations
duration_summary <- lena_first_two_dates %>%
  group_by(ExternalReferenceID) %>%
  summarize(
    Total_Duration = sum(Duration_Hours, na.rm = TRUE),
    Average_Duration = mean(Duration_Hours, na.rm = TRUE),
    Min_Duration = min(Duration_Hours, na.rm = TRUE),
    Max_Duration = max(Duration_Hours, na.rm = TRUE)
  )

# Step 3: Calculate overall statistics
overall_average_duration <- mean(duration_summary$Average_Duration)
overall_min_duration <- min(duration_summary$Min_Duration)
overall_max_duration <- max(duration_summary$Max_Duration)

# Print the results
cat("Overall Average duration:", overall_average_duration, "hours\n")
cat("Overall Minimum duration:", overall_min_duration, "hours\n")
cat("Overall Maximum duration:", overall_max_duration, "hours\n")

## DELEETETTETE

#################### I am combining the first timepoint (2 dates) together 
# Convert RecordingDate to date format
lena_first_two_dates$RecordingDate <- as.Date(lena_first_two_dates$RecordingDate, format = "%m/%d/%Y")

# Group by participant, select the earliest date, and calculate the sum of counts
lena_sum <- lena_first_two_dates %>%
  group_by(ExternalReferenceID, Recording_Gender, Recording_DOB) %>%
  summarise(
    RecordingDate = min(RecordingDate),
    CT_COUNT_sum = sum(CT_COUNT, na.rm = TRUE),
    AWC_COUNT_sum = sum(AWC_COUNT, na.rm = TRUE),
    CV_COUNT_sum = sum(CV_COUNT, na.rm = TRUE)
  ) %>%
  ungroup()

# View the result
print(lena_sum)





#################### DELETE
# Convert RecordingDate to date format
lena$RecordingDate <- as.Date(lena$RecordingDate, format = "%m/%d/%Y")

# Group by participant and select the first two earliest dates
lena_sum <- lena %>%
  group_by(ExternalReferenceID, Recording_Gender, Recording_DOB) %>%
  arrange(RecordingDate) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 2) %>%
  ungroup() %>%
  select(-rank) %>%
  summarise(
    CT_COUNT_sum = sum(CT_COUNT, na.rm = TRUE),
    AWC_COUNT_sum = sum(AWC_COUNT, na.rm = TRUE),
    CV_COUNT_sum = sum(CV_COUNT, na.rm = TRUE)
  )



#####################

#load if you have not
library(dplyr)

# Summarize the lena_summary dataframe to get the sum of counts per participant
lena_sum <- lena %>%
  group_by(ExternalReferenceID, Recording_Gender, Recording_DOB, RecordingDate) %>%
  summarise(
    CT_COUNT_sum = sum(CT_COUNT, na.rm = TRUE),
    AWC_COUNT_sum = sum(AWC_COUNT, na.rm = TRUE),
    CV_COUNT_sum = sum(CV_COUNT, na.rm = TRUE)
  ) %>%
  ungroup()

library(dplyr)

# Convert RecordingDate to date format
lena_summary$RecordingDate <- as.Date(lena_summary$RecordingDate, format = "%m/%d/%Y")

# Group by participant and select the first two earliest dates
lena_sum <- lena_summary %>%
  group_by(ExternalReferenceID) %>%
  arrange(RecordingDate) %>%
  slice_head(n = 2) %>%
  ungroup()

# View the result
print(lena_sum)


#save as a summary csv 
write.csv(lena_sum, 
          file.path('raw-csv', 'Summary', '1.lena_Summary.csv'),
          row.names=FALSE)

