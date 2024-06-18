# Clear previous data
rm(list = ls())

#Load data
final_df <- read.csv(file.path('raw-csv', '5.S_CDI_FINALRAW.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)

#load the word matching data
word_name <- read.csv(file.path('source','CDI', 'word_code_CDI.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)


# Merge final_df with word_name for S_word
merged_S_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)


# Merge final_df with word_name for B_word
merged_B_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)



########################
# Merge final_df with word_name for S_word
merged_S_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)

# Merge final_df with word_name for B_word
merged_B_word <- merge(final_df, word_name, by.x = "item_code", by.y = "Bil_code", all.x = TRUE)

# Add the SP_Word column from word_name to final_df as S_word
final_df$S_word <- merged_S_word$SP_Word

# Add the BQ_Word column from word_name to final_df as B_word
final_df$B_word <- merged_B_word$BQ_Word





