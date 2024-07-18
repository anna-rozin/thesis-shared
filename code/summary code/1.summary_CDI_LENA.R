
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



