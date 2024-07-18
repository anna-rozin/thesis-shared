# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts.csv'), header=TRUE, stringsAsFactors=FALSE)


# Print all column names of the both_counts data frame
print(names(both_counts))



####### making scatter plots 

library(ggplot2)

# Define a function to calculate R-squared
calculate_r_squared <- function(x, y) {
  model <- lm(y ~ x)
  summary <- summary(model)
  r_squared <- summary$r.squared
  return(r_squared)
}

# Define CDI measures and Lena measures
cdi_measures <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                  "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend",
                  "skill_total_produce", "skill_concept_produce")

lena_measures <- c("CT_COUNT_Avg", "CV_COUNT_Avg", "AWC_COUNT_Avg")

# Define the path
output_path <- file.path('raw-csv', 'Analysis', 'Graphs')

# Create the directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Create scatter plots for all combinations
for (cdi_measure in cdi_measures) {
  for (lena_measure in lena_measures) {
    # Create a dataframe for the current combination
    data <- data.frame(x = both_counts[[lena_measure]], y = both_counts[[cdi_measure]], ID_lab = both_counts$ID_lab)
    
    # Calculate R-squared
    r_squared <- calculate_r_squared(both_counts[[lena_measure]], both_counts[[cdi_measure]])
    
    # Create the scatter plot
    plot <- ggplot(data, aes(x = x, y = y, label = ID_lab)) +
      geom_point() +                       # Add scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Add line of best fit (linear regression)
      labs(x = lena_measure, y = cdi_measure, title = paste("Scatter Plot of", cdi_measure, "vs", lena_measure)) +
      geom_text(hjust = 0, vjust = 0, nudge_x = 0.5, nudge_y = 0.5) +  # Adjust label position
      annotate("text", x = max(data$x), y = min(data$y), label = paste("R-squared:", round(r_squared, 3)), hjust = 1, vjust = 0)  # Add R-squared value
    
    # Save the plot
    filename <- file.path(output_path, paste0("Scatter_", cdi_measure, "_vs_", lena_measure, ".png"))
    ggsave(filename, plot, width = 8, height = 6)
  }
}




#### TIME 2 

# Clear previous data
rm(list = ls())

# Load data
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'), header=TRUE, stringsAsFactors=FALSE)

both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts_T2.csv'), header=TRUE, stringsAsFactors=FALSE)


####### making scatter plots 

library(ggplot2)

# Define a function to calculate R-squared
calculate_r_squared <- function(x, y) {
  model <- lm(y ~ x)
  summary <- summary(model)
  r_squared <- summary$r.squared
  return(r_squared)
}

# Define CDI measures and Lena measures
cdi_measures <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                  "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend",
                  "skill_total_produce", "skill_concept_produce")

lena_measures <- c("CT_COUNT_Avg", "CV_COUNT_Avg", "AWC_COUNT_Avg")

# Define the path
output_path <- file.path('raw-csv', 'Analysis', 'Graphs', 'T2')

# Create the directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Create scatter plots for all combinations
for (cdi_measure in cdi_measures) {
  for (lena_measure in lena_measures) {
    # Create a dataframe for the current combination
    data <- data.frame(x = both_counts[[lena_measure]], y = both_counts[[cdi_measure]], ID_lab = both_counts$ID_lab)
    
    # Calculate R-squared
    r_squared <- calculate_r_squared(both_counts[[lena_measure]], both_counts[[cdi_measure]])
    
    # Create the scatter plot
    plot <- ggplot(data, aes(x = x, y = y, label = ID_lab)) +
      geom_point() +                       # Add scatter plot points
      geom_smooth(method = "lm", se = FALSE) +  # Add line of best fit (linear regression)
      labs(x = lena_measure, y = cdi_measure, title = paste("Scatter Plot of", cdi_measure, "vs", lena_measure)) +
      geom_text(hjust = 0, vjust = 0, nudge_x = 0.5, nudge_y = 0.5) +  # Adjust label position
      annotate("text", x = max(data$x), y = min(data$y), label = paste("R-squared:", round(r_squared, 3)), hjust = 1, vjust = 0)  # Add R-squared value
    
    # Save the plot
    filename <- file.path(output_path, paste0("Scatter_", cdi_measure, "_vs_", lena_measure, ".png"))
    ggsave(filename, plot, width = 8, height = 6)
  }
}







####### removing participants 

# Define the IDs to be removed
ids_to_remove <- c("6937B", "7135", "6903B")

# Remove rows with the specified IDs
both_counts_filtered <- both_counts %>% 
  filter(!ID_lab %in% ids_to_remove)

# Define the path
output_path <- file.path('raw-csv', 'Analysis', 'Graphs')

# Create the directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Create scatter plots for all combinations
for (cdi_measure in cdi_measures) {
  for (lena_measure in lena_measures) {
    # Remove specified participants
    both_counts_filtered <- both_counts %>% 
      filter(!ID_lab %in% c("6937B", "7135", "6903B"))
    
    # Create a dataframe for the current combination
    data <- data.frame(x = both_counts_filtered[[lena_measure]], 
                       y = both_counts_filtered[[cdi_measure]], 
                       ID_lab = both_counts_filtered$ID_lab)
    
    # Check if data has non-zero rows
    if (nrow(data) > 0) {
      # Calculate R-squared
      r_squared <- calculate_r_squared(both_counts_filtered[[lena_measure]], 
                                       both_counts_filtered[[cdi_measure]])
      
      # Create the scatter plot
      plot <- ggplot(data, aes(x = x, y = y, label = ID_lab)) +
        geom_point() +                       # Add scatter plot points
        geom_smooth(method = "lm", se = FALSE) +  # Add line of best fit (linear regression)
        labs(x = lena_measure, y = cdi_measure, 
             title = paste("Scatter Plot of", cdi_measure, "vs", lena_measure)) +
        geom_text(hjust = 0, vjust = 0, nudge_x = 0.5, nudge_y = 0.5) +  # Adjust label position
        annotate("text", x = max(data$x), y = min(data$y), 
                 label = paste("R-squared:", round(r_squared, 3)), hjust = 1, vjust = 0)  # Add R-squared value
      
      # Save the plot
      filename <- file.path(output_path, 
                            paste0("Scatter_", cdi_measure, "_vs_", lena_measure, ".png"))
      ggsave(filename, plot, width = 8, height = 6)
    } else {
      cat("Data is empty for", cdi_measure, "vs", lena_measure, ". Skipping...\n")
      # Print out which measures have zero rows of data
      cat("lena_measure:", lena_measure, "\n")
      cat("cdi_measure:", cdi_measure, "\n")
    }
  }
}



############################# DELETE BELOW 
# Print column names of both_counts
cat("Column names of both_counts:\n")
print(names(both_counts))

# Print lena_measures
cat("Column names in lena_measures:\n")
print(lena_measures)


# Summary statistics for cdi_measures
cat("Summary statistics for cdi_measures:\n")
print(summary(both_counts[cdi_measures]))

# Summary statistics for lena_measures
cat("Summary statistics for lena_measures:\n")
print(summary(both_counts[lena_measures]))


# Check for missing values

any_missing <- anyNA(both_counts[c("skill_concept_produce", "skill_total_produce")])
if (any_missing) {
  cat("Missing values detected in 'skill_concept_produce' or 'skill_total_produce'.\n")
  cat("Summary of missing values:\n")
  print(summary(both_counts[c("skill_concept_produce", "skill_total_produce")]))
} else {
  cat("No missing values detected in 'skill_concept_produce' or 'skill_total_produce'.\n")
}

############## with line of best fit 

# Define CDI measures and Lena measures
cdi_measures <- c("skill_es_comprehend", "skill_eu_comprehend", "skill_es_produce", 
                  "skill_eu_produce", "skill_total_comprehend", "skill_concept_comprehend")
lena_measures <- c("CT_COUNT_sum", "CV_COUNT_sum", "AWC_COUNT_sum")

# Define the path
output_path <- file.path('raw-csv', 'Analysis', 'Graphs')

# Create the directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Create scatter plots for all combinations
for (cdi_measure in cdi_measures) {
  for (lena_measure in lena_measures) {
    # Save the scatter plot with the specified file name and path
    filename <- file.path(output_path, paste0("Scatter_", cdi_measure, "_vs_", lena_measure, ".png"))
    png(filename)
    
    # Create the scatter plot
    plot(both_counts[[lena_measure]], both_counts[[cdi_measure]], 
         xlab = lena_measure, ylab = cdi_measure,
         main = paste("Scatter Plot of", cdi_measure, "vs", lena_measure))
    
    # Add labels for ID_lab
    text(both_counts[[lena_measure]], both_counts[[cdi_measure]], 
         labels = both_counts$ID_lab, pos = 3, cex = 0.7)
    
    # Add the line of best fit
    abline(lm(both_counts[[cdi_measure]] ~ both_counts[[lena_measure]]), col = "red")
    
    # Close the PNG device
    dev.off()
  }
}



#### attempting 

plot(both_counts$skill_total_comprehend, both_counts$AWC_COUNT_sum, 
     xlab = "Skill Total Comprehend", ylab = "AWC_COUNT",
     main = "Scatter Plot of Skill Total Comprehend vs AWC_COUNT")
text(both_counts$skill_total_comprehend, both_counts$AWC_COUNT_sum, 
     labels = both_counts$ID_lab, pos = 3, cex = 0.7)


plot(both_counts$skill_total_comprehend, both_counts$AWC_COUNT_sum, 
     xlab = "Skill Total Comprehend", ylab = "AWC_COUNT",
     main = "Scatter Plot of Skill Total Comprehend vs AWC_COUNT")

# Save the scatter plot as a PNG file with the specified file name and path
png("'raw-csv', 'Analysis', 'AWC_TC.png'")

# Create the scatter plot with flipped axes
plot(both_counts$AWC_COUNT_sum, both_counts$skill_total_comprehend, 
     xlab = "AWC_COUNT_sum", ylab = "Skill Total Comprehend",
     main = "Scatter Plot of AWC_COUNT_sum vs Skill Total Comprehend")

# Close the PNG device
dev.off()


