# Clear previous data
rm(list = ls())

# Load data for T1
bil_both_counts_T1 <- read.csv(file.path('raw-csv', 'Analysis', 'bil_both_counts_T1.csv'), header=TRUE, stringsAsFactors=FALSE)
mono_both_counts_T1 <- read.csv(file.path('raw-csv', 'Analysis', 'mono_both_counts_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load summary data with age_CDI 
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)


#### mono age 

# Match age_CDI for T1 (token = 1)
mono_both_counts_T1 <- merge(mono_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

##### bil age
# Match age_CDI for T1 (token = 1)
bil_both_counts_T1 <- merge(bil_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)


#### merged T1 mono/bil 
# Merge mono_both_counts_T1 and bil_both_counts_T1
both_counts_T1 <- rbind(mono_both_counts_T1, bil_both_counts_T1)



###### FRANKENSTEIN MEASURE 
# Load necessary package
library(dplyr)

# Calculate weighted averages for T1
both_counts_T1$L1_AWC_weighted <- both_counts_T1$AWC_COUNT_Avg * (both_counts_T1$L1_p / 100)
both_counts_T1$L2_AWC_weighted <- both_counts_T1$AWC_COUNT_Avg * (both_counts_T1$L2_p / 100)

# Subset T1 data and remove duplicates
data_T1 <- both_counts_T1 %>%
  select(ID_lab, L1, L2, L1_p, L2_p, L1_AWC_weighted, L2_AWC_weighted) %>%
  distinct()

# Print the T1 data to verify
print(data_T1)
