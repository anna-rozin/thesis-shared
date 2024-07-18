
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


