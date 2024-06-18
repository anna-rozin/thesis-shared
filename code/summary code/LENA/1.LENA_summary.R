### Set working directory and load data

# Clear previous data
rm(list = ls())

lena <- read.csv(file.path('raw-csv', 'lena_corrected_IDs.csv'),
                header=TRUE,
                stringsAsFactors=FALSE)
###___________________________

# Print all column names in the lena dataframe
print("Column names in lena:")
print(colnames(lena))


library(dplyr)

# Select the desired columns to create summary_lena
summary_lena <- lena[, c("Recording.ID", "ChildKey", "ExternalReferenceID", "Recording_Gender", "Recording_DOB", "RecordingDate",
                         "StartTime", "EndTime","ParticipantID", "CT_COUNT", "AWC_COUNT", "CV_COUNT")]
#save as a summary csv 

write.csv(summary_lena, 
          file.path('raw-csv', 'Summary', 'lena_Summary.csv'),
          row.names=FALSE)
