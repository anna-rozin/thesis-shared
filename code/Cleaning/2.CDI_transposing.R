#Load data
dat <- read.csv(file.path('raw-csv', '1.B_CDI_nontransposed.csv'),
                 header=TRUE,
                 stringsAsFactors=FALSE)

### Creating two dataframes###

# Identify columns to transpose based on a pattern (starting with "VOC", "COM", or "PROTEM")
cols_to_transpose <- grep("^VOC|^COM|^PROTEM|^GES|^JUE|^ACC", colnames(dat), value = TRUE)

# Keep all columns except those to transpose
cols_to_keep <- setdiff(colnames(dat), cols_to_transpose)

# Create two new data frames: df1 (part to transpose) and df2 (part to keep as is)
df1 <- dat[, c("ID", cols_to_transpose)]
df2 <- dat[, c("ID", cols_to_keep)]

# Print the first few rows of each data frame to verify
head(df1)
head(df2)


### Transposing the data ###

# Load the tidyr package
library(tidyr)

# Transpose df1 while keeping IDs intact
df1_transposed <- pivot_longer(df1, 
                               cols = -ID,  # Exclude the ID column
                               names_to = "item",  # Name of the new column storing original column names
                               values_to = "value"  # Name of the new column storing cell values
)


#### merging df1 and df2 ###
#Assuming 'df1_transposed' is your transposed data frame and 'df2' is the other data frame

# Merge df2 with transposed df1 based on the common ID column
final_df <- merge(df2, df1_transposed, by = "ID")

#save as transposed csv

write.csv(final_df, 
          file.path('raw-csv', '2.B_CDI_transposed.csv'),
          row.names=FALSE)

