
######## do not need to use this, as we have the averages already in 1. lena summary


# Clear previous data
rm(list = ls())

# Load necessary libraries
library(ggplot2)
library(dplyr)


# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data
cdi_avg_rng <- read.csv(file.path('raw-csv', 'Analysis', '10.CDI_averages.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data
cdi_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.CDI_participants.csv'), header=TRUE, stringsAsFactors=FALSE)

##### LENA ##########
# sum the counts 

# Summarize CT_COUNT, CV_COUNT, and AWC_COUNT by participant
lena_counts <- summary_dat %>%
  group_by(ID_lab) %>%
  summarise(
    CT_COUNT_sum = sum(CT_COUNT_sum, na.rm = TRUE),
    CV_COUNT_sum = sum(CV_COUNT_sum, na.rm = TRUE),
    AWC_COUNT_sum = sum(AWC_COUNT_sum, na.rm = TRUE)
  )

# Assuming df is your dataframe
lena_counts <- lena_counts[-c(6, 17), ]


#save as CSV
write.csv(lena_counts, 
          file.path('raw-csv', 'Analysis', '9.lena_counts.csv'),
          row.names=FALSE)


