# Clear previous data
rm(list = ls())

# Load the data T1
bil_both_counts_T1 <- read.csv(file.path('raw-csv', 'Analysis', 'bil_both_counts_T1.csv'), header=TRUE, stringsAsFactors=FALSE)
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)

# T2
both_counts_T2 <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts_L1L2_T2.csv'), header=TRUE, stringsAsFactors=FALSE)
summary_dat_T2 <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'), header=TRUE, stringsAsFactors=FALSE)



# Select the ID_lab and age_CDI columns from summary_dat
summary_subset <- summary_dat[, c("ID_lab", "age_CDI")]

# Merge age_CDI into both_counts based on ID_lab
both_counts <- merge(both_counts, summary_subset, by = "ID_lab")

# Assuming 'ID_lab' is the common identifier in both datasets
both_counts_T2 <- merge(both_counts_T2, summary_dat_T2[c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

# Assuming both_counts and both_counts_T2 have the same column structure
# Merge by appending rows
merged_both_counts <- rbind(both_counts, both_counts_T2)


# Update Mono_Bil values for specific participants
merged_both_counts$Mono_Bil[both_counts$ID_lab == "6925"] <- "Bil"
merged_both_counts$Mono_Bil[both_counts$ID_lab == "7006B"] <- "Mono"


###### RQ 1 analysis #######
# Load necessary packages if not already loaded

library(car)


##### L1 comprehension FORMULA 
# Define the formula for L1 comprehension regression
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 comprehension
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Print the summary of the L1 comprehension regression model
summary(model_L1)

# L1 comprehension graph 
# Load the ggplot2 package
library(ggplot2)

# Fit the regression model for L1 comprehension
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Add residuals to the data frame
merged_both_counts$residuals_L1 <- residuals(model_L1)

# Create a scatter plot of residuals against AWC_COUNT_Avg, colored by Mono_Bil
ggplot(data = merged_both_counts, aes(x = AWC_COUNT_Avg, y = residuals_L1, color = Mono_Bil)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L1 Comprehension Regression Model",
       x = "Average Adult Word Count",
       y = "Residuals of L1 Comprehension",
       color = "Bilingual Status") +
  theme_minimal()



# interaction 
# Define the formula with interaction for L1 comprehension
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg * Mono_Bil + age_CDI

# Run the multiple linear regression for L1 comprehension
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Print the summary of the L1 comprehension regression model
summary(model_L1)

###### L2 Comprehension FORMULA 

# Subset the data to include only bilingual participants
bilingual_data <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formula for L2 comprehension regression
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension on bilingual data
model_L2 <- lm(formula_L2, data = bilingual_data)

# Print the summary of the L2 comprehension regression model
summary(model_L2)

# interaction 


# L2 comprehension graph 
# Fit the regression model for L2 comprehension on bilingual data
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI
model_L2 <- lm(formula_L2, data = bilingual_data)

# Add residuals to the data frame
bilingual_data$residuals_L2 <- residuals(model_L2)

# Create a scatter plot of residuals against AWC_COUNT_Avg
ggplot(data = bilingual_data, aes(x = AWC_COUNT_Avg, y = residuals_L2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L2 Comprehension Regression Model (Bilingual Participants)",
       x = "Average Adult Word Count",
       y = "Residuals of L2 Comprehension") +
  theme_minimal()


##### L1 production formula 
# Define the formula for L1 production
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# Summarize the results
summary(model_L1_prod)

# Load the ggplot2 package
library(ggplot2)

# Add residuals to the data frame
merged_both_counts$residuals_L1_prod <- residuals(model_L1_prod)

# Create a scatter plot of residuals against AWC_COUNT_Avg, colored by Mono_Bil
ggplot(data = merged_both_counts, aes(x = AWC_COUNT_Avg, y = residuals_L1_prod, color = Mono_Bil)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L1 Production Regression Model",
       x = "Average Adult Word Count",
       y = "Residuals of L1 Production",
       color = "Bilingual Status") +
  theme_minimal()


#### L2 production formula 
# Load the ggplot2 package
library(ggplot2)

# Subset the data to include only bilingual participants
bilingual_data <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formula for L2 production regression
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 production on bilingual data
model_L2_prod <- lm(formula_L2_prod, data = bilingual_data)

# Print the summary of the L2 production regression model
summary(model_L2_prod)

# Add residuals to the data frame
bilingual_data$residuals_L2_prod <- residuals(model_L2_prod)

# Create a scatter plot of residuals against AWC_COUNT_Avg
ggplot(data = bilingual_data, aes(x = AWC_COUNT_Avg, y = residuals_L2_prod)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L2 Production Regression Model (Bilingual Participants)",
       x = "Average Adult Word Count",
       y = "Residuals of L2 Production") +
  theme_minimal()

########## RQ 2 + CT (BEYOND AWC)
#L1 comprehension

# Define the formula for L1 comprehension regression
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 comprehension
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Print the summary of the L1 comprehension regression model
summary(model_L1)

# Add residuals to the data frame
merged_both_counts$residuals_L1 <- residuals(model_L1)

# Create a scatter plot of residuals against AWC_COUNT_Avg, colored by Mono_Bil
ggplot(data = merged_both_counts, aes(x = AWC_COUNT_Avg, y = residuals_L1, color = Mono_Bil)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L1 Comprehension Regression Model",
       x = "Average Adult Word Count",
       y = "Residuals of L1 Comprehension",
       color = "Bilingual Status") +
  theme_minimal()


######## L2 comprehension

# Subset the data to include only bilingual participants
bilingual_data <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formula for L2 comprehension regression
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension on bilingual data
model_L2 <- lm(formula_L2, data = bilingual_data)

# Print the summary of the L2 comprehension regression model
summary(model_L2)

# Add residuals to the data frame
bilingual_data$residuals_L2 <- residuals(model_L2)

# Create a scatter plot of residuals against AWC_COUNT_Avg
ggplot(data = bilingual_data, aes(x = AWC_COUNT_Avg, y = residuals_L2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L2 Comprehension Regression Model (Bilingual Participants)",
       x = "Average Adult Word Count",
       y = "Residuals of L2 Comprehension") +
  theme_minimal()

## L1 production 


# Define the formula for L1 production regression
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# Print the summary of the L1 production regression model
summary(model_L1_prod)

# Add residuals to the data frame
merged_both_counts$residuals


# Add residuals to the data frame
merged_both_counts$residuals_L1_prod <- residuals(model_L1_prod)

# Create a scatter plot of residuals against AWC_COUNT_Avg, colored by Mono_Bil
ggplot(data = merged_both_counts, aes(x = AWC_COUNT_Avg, y = residuals_L1_prod, color = Mono_Bil)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Residuals of L1 Production Regression Model",
       x = "Average Adult Word Count",
       y = "Residuals of L1 Production",
       color = "Bilingual Status") +
  theme_minimal()




########## POWERPOINT
library(ggplot2)
library(reshape2)

# Subset the data to include only bilingual participants for L2 comprehension
merged_both_counts_bil <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formulas and run the multiple linear regressions
# L1 comprehension
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L1 <- lm(formula_L1, data = merged_both_counts)

# L2 comprehension (only bilingual participants)
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI
model_L2 <- lm(formula_L2, data = merged_both_counts_bil)

# Extract predicted values from the models
predicted_L1 <- predict(model_L1, newdata = merged_both_counts)
predicted_L2 <- predict(model_L2, newdata = merged_both_counts_bil)

# Create a data frame for plotting
plot_data <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  L1_comprehension = predicted_L1,
  L2_comprehension = NA, # Initialize L2 comprehension with NA
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Replace L2 comprehension values only for bilingual participants
plot_data$L2_comprehension[merged_both_counts$Mono_Bil == "Bil"] <- predicted_L2

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_long <- melt(plot_data, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting L1 and L2 comprehension
ggplot(plot_data_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("L1_comprehension" = "red", "L2_comprehension" = "blue"),
                     labels = c("L1", "L2")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Comprehension") +
  theme_minimal()


# production
library(ggplot2)
library(reshape2)

# Subset the data to include only bilingual participants for L2 production
merged_both_counts_bil <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formulas and run the multiple linear regressions
# L1 production
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# L2 production (only bilingual participants)
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + age_CDI
model_L2_prod <- lm(formula_L2_prod, data = merged_both_counts_bil)

# Extract predicted values from the models
predicted_L1_prod <- predict(model_L1_prod, newdata = merged_both_counts)
predicted_L2_prod <- predict(model_L2_prod, newdata = merged_both_counts_bil)

# Create a data frame for plotting
plot_data_prod <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  L1_production = predicted_L1_prod,
  L2_production = NA, # Initialize L2 production with NA
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Replace L2 production values only for bilingual participants
plot_data_prod$L2_production[merged_both_counts$Mono_Bil == "Bil"] <- predicted_L2_prod

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_prod_long <- melt(plot_data_prod, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Production_Type", value.name = "Production")

# Plotting L1 and L2 production
ggplot(plot_data_prod_long, aes(x = age_CDI, y = Production, color = Production_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("L1_production" = "red", "L2_production" = "blue"),
                     labels = c("L1", "L2")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Production", color = "Production Type", linetype = "Mono/Bil") +
  ggtitle("Production") +
  theme_minimal()



######### LENA

library(ggplot2)
library(dplyr)  # for data manipulation

# Ensure each participant ID is counted only once
unique_participants <- merged_both_counts %>%
  distinct(ID_lab, .keep_all = TRUE)  # Keep the first instance of each unique ID

# Plot histogram of AWC_COUNT_Avg
ggplot(unique_participants, aes(x = AWC_COUNT_Avg)) +
  geom_histogram(binwidth = 300, fill = "skyblue", color = "black") +
  labs(x = "AWC Count Average", y = "Quantity of Participants") +
  ggtitle("Histogram of AWC Count Average (Unique Participants)") +
  theme_minimal()


library(ggplot2)
library(dplyr)  # for data manipulation

# Ensure each participant ID is counted only once
unique_participants <- merged_both_counts %>%
  distinct(ID_lab, .keep_all = TRUE)  # Keep the first instance of each unique ID

# Plot histogram of CT_COUNT_Avg starting at 0 with binwidth of 20
ggplot(unique_participants, aes(x = CT_COUNT_Avg)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black", 
                 breaks = seq(0, max(unique_participants$CT_COUNT_Avg) + 10, by = 10)) +
  labs(x = "CT Count Average", y = "Quantity of Participants") +
  ggtitle("Histogram of CT Count Average (Unique Participants)") +
  theme_minimal()


#### attempts of creating a chart 
library(ggplot2)
library(dplyr)

# Example data (replace with your actual data and models)
# This example uses mock coefficients
coefficients_data <- data.frame(
  Predictor = c("AWC_COUNT_Avg", "AWC_COUNT_Avg", "CT_COUNT_Avg", "CT_COUNT_Avg", "Bilingual_Status", "Age"),
  Dependent_Variable = c("L1_comprehension", "L2_comprehension", "L1_vocabulary", "L2_vocabulary", "Total_comprehension", "Conceptual_comprehension"),
  Coefficient = c(0.3, -0.2, 0.4, -0.1, 0.1, 0.2)
)

# Assign colors based on positive (blue) or negative (red) correlation
coefficients_data$Color <- ifelse(coefficients_data$Coefficient >= 0, "blue", "red")

# Plotting coefficients
ggplot(coefficients_data, aes(x = Predictor, y = Dependent_Variable, label = ifelse(Coefficient >= 0, "+", "-"), color = Color)) +
  geom_text(size = 6) +
  scale_color_manual(values = c("blue" = "blue", "red" = "red")) +
  labs(
    title = "Correlation between Predictors and Dependent Variables",
    x = "Predictors",
    y = "Dependent Variables",
    color = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##### RQ 3 
# Conceptual Comprehension
# Define the formula for conceptual vocab comprehension
formula_conceptual_comprehend <- L1_conceptual_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comprehend)


# Extract residuals and fitted values
residuals_comprehend <- residuals(model_conceptual_comprehend)
fitted_comprehend <- fitted.values(model_conceptual_comprehend)

# Create a data frame for plotting residuals
plot_data_comprehend <- data.frame(
  fitted = fitted_comprehend,
  residuals = residuals_comprehend
)

# Plotting residuals for conceptual comprehension
ggplot(plot_data_comprehend, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals Plot for Conceptual Comprehension") +
  theme_minimal()


### Conceptual Production
# Define the formula for conceptual vocab production
formula_conceptual_prod <- L1_conceptual_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod <- lm(formula_conceptual_prod, data = merged_both_counts)

# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod)

# Extract residuals and fitted values
residuals_prod <- residuals(model_conceptual_prod)
fitted_prod <- fitted.values(model_conceptual_prod)

# Create a data frame for plotting residuals
plot_data_prod <- data.frame(
  fitted = fitted_prod,
  residuals = residuals_prod
)

# Plotting residuals for conceptual production
ggplot(plot_data_prod, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals Plot for Conceptual Production") +
  theme_minimal()

### POWER POINT 

library(ggplot2)
library(reshape2)

# Subset the data to include only bilingual participants for L2 comprehension and production
merged_both_counts_bil <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formulas and run the multiple linear regressions
# Conceptual Comprehension
formula_comprehend <- skill_concept_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_comprehend <- lm(formula_comprehend, data = merged_both_counts)

# Conceptual Production
formula_produce <- skill_concept_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_produce <- lm(formula_produce, data = merged_both_counts)

# Extract predicted values from the models
predicted_comprehend <- predict(model_comprehend, newdata = merged_both_counts)
predicted_produce <- predict(model_produce, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Comprehend = predicted_comprehend,
  Produce = predicted_produce,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_long <- melt(plot_data, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Skill_Type", value.name = "Skill")

# Plotting comprehension and production
ggplot(plot_data_long, aes(x = age_CDI, y = Skill, color = Skill_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Comprehend" = "red", "Produce" = "blue"),
                     labels = c("Comprehension", "Production")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Skill Level", color = "Skill Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual Comprehension and Production") +
  theme_minimal()


library(ggplot2)
library(reshape2)

# Run the multiple linear regressions
# Conceptual Comprehension
formula_conceptual_comprehend <- skill_concept_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)

# Total Comprehension
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_comprehend <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Comprehend = predicted_conceptual_comprehend,
  Total_Comprehend = predicted_total_comprehend,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_comprehend_long <- melt(plot_data_comprehend, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting Conceptual Comprehension and Total Comprehension
ggplot(plot_data_comprehend_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Comprehend" = "red", "Total_Comprehend" = "blue"),
                     labels = c("Conceptual Comprehension", "Total Comprehension")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Comprehension") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal() 


### 

library(ggplot2)
library(reshape2)

# Run the multiple linear regressions
# Conceptual Production
formula_conceptual_produce <- skill_concept_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_conceptual_produce <- lm(formula_conceptual_produce, data = merged_both_counts)

# Total Production
formula_total_produce <- skill_total_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_produce <- lm(formula_total_produce, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_produce <- predict(model_conceptual_produce, newdata = merged_both_counts)
predicted_total_produce <- predict(model_total_produce, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_produce <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Produce = predicted_conceptual_produce,
  Total_Produce = predicted_total_produce,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_produce_long <- melt(plot_data_produce, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Production_Type", value.name = "Production")

# Plotting Conceptual Production and Total Production
ggplot(plot_data_produce_long, aes(x = age_CDI, y = Production, color = Production_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Produce" = "red", "Total_Produce" = "blue"),
                     labels = c("Conceptual Production", "Total Production")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Production", color = "Production Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Production") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal() 


# Run the multiple linear regressions without interaction
# Conceptual Production
formula_conceptual_produce <- skill_concept_produce ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_conceptual_produce <- lm(formula_conceptual_produce, data = merged_both_counts)

# Total Production
formula_total_produce <- skill_total_produce ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_total_produce <- lm(formula_total_produce, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_produce <- predict(model_conceptual_produce, newdata = merged_both_counts)
predicted_total_produce <- predict(model_total_produce, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_produce <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Produce = predicted_conceptual_produce,
  Total_Produce = predicted_total_produce,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_produce_long <- melt(plot_data_produce, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Production_Type", value.name = "Production")

# Plotting Conceptual Production and Total Production without interaction
ggplot(plot_data_produce_long, aes(x = age_CDI, y = Production, color = Production_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Produce" = "red", "Total_Produce" = "blue"),
                     labels = c("Conceptual Production", "Total Production")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Production", color = "Production Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Production") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal()



library(ggplot2)
library(reshape2)

# Run the multiple linear regressions
# Total Comprehension
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Total Production
formula_total_prod <- skill_total_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_prod <- lm(formula_total_prod, data = merged_both_counts)

# Extract predicted values from the models
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)
predicted_total_prod <- predict(model_total_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_total <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Total_Comprehend = predicted_total_comprehend,
  Total_Produce = predicted_total_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_total_long <- melt(plot_data_total, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Skill_Type", value.name = "Skill")

# Plotting Total Comprehension and Total Production
ggplot(plot_data_total_long, aes(x = age_CDI, y = Skill, color = Skill_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Total_Comprehend" = "red", "Total_Produce" = "blue"),
                     labels = c("Total Comprehension", "Total Production")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Skill", color = "Skill Type", linetype = "Mono/Bil") +
  ggtitle("Total Comprehension and Total Production by Age and Group") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom")



library(ggplot2)
library(reshape2)

# Total Comprehension
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Summary of Total Comprehension regression
summary_total_comprehend <- summary(model_total_comprehend)

# Plot Total Comprehension with residuals
plot_data_comprehend <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Total_Comprehend = predict(model_total_comprehend, newdata = merged_both_counts),
  Residuals = residuals(model_total_comprehend)
)

# Plot Total Comprehension
plot_comprehend <- ggplot(plot_data_comprehend, aes(x = age_CDI)) +
  geom_line(aes(y = Total_Comprehend), color = "blue", size = 1) +
  geom_point(aes(y = Residuals), color = "red", size = 1) +
  labs(x = "Age (CDI)", y = "Total Comprehension / Residuals", title = "Total Comprehension with Residuals") +
  theme_minimal()

# Total Production
formula_total_prod <- skill_total_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_total_prod <- lm(formula_total_prod, data = merged_both_counts)

# Summary of Total Production regression
summary_total_prod <- summary(model_total_prod)

# Plot Total Production with residuals
plot_data_produce <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Total_Produce = predict(model_total_prod, newdata = merged_both_counts),
  Residuals = residuals(model_total_prod)
)

# Plot Total Production
plot_produce <- ggplot(plot_data_produce, aes(x = age_CDI)) +
  geom_line(aes(y = Total_Produce), color = "green", size = 1) +
  geom_point(aes(y = Residuals), color = "orange", size = 1) +
  labs(x = "Age (CDI)", y = "Total Production / Residuals", title = "Total Production with Residuals") +
  theme_minimal()

# Show regression summaries
print(summary_total_comprehend)
print(summary_total_prod)

# Display plots
plot_comprehend
plot_produce



# Conceptual Comprehension
formula_conceptual_comprehend <- skill_concept_comprehend ~ AWC_COUNT_Avg + Mono_Bil + AWC_COUNT_Avg:Mono_Bil + age_CDI
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)

# Total Comprehension
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg + Mono_Bil + AWC_COUNT_Avg:Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_comprehend <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Comprehend = predicted_conceptual_comprehend,
  Total_Comprehend = predicted_total_comprehend,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_comprehend_long <- melt(plot_data_comprehend, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting Conceptual Comprehension and Total Comprehension
ggplot(plot_data_comprehend_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Comprehend" = "red", "Total_Comprehend" = "blue"),
                     labels = c("Conceptual Comprehension", "Total Comprehension")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Comprehension") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal()


# Conceptual Comprehension
formula_conceptual_comprehend <- skill_concept_comprehend ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)

# Total Comprehension
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_comprehend <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Comprehend = predicted_conceptual_comprehend,
  Total_Comprehend = predicted_total_comprehend,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_comprehend_long <- melt(plot_data_comprehend, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting Conceptual Comprehension and Total Comprehension
ggplot(plot_data_comprehend_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Comprehend" = "red", "Total_Comprehend" = "blue"),
                     labels = c("Conceptual Comprehension", "Total Comprehension")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Comprehension") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal()

library(ggplot2)
library(reshape2)  # For melt function
library(ggplot2)
library(reshape2)  # For melt function

# Filter data for Conceptual Comprehension (exclude monolinguals)
merged_both_counts_bil_conceptual <- merged_both_counts[merged_both_counts$Mono_Bil == "Bil", ]

# Conceptual Comprehension model
formula_conceptual_comprehend <- skill_concept_comprehend ~ AWC_COUNT_Avg + age_CDI
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts_bil_conceptual)

# Total Comprehension model (include both mono and bil)
formula_total_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Extract predicted values from the models
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_comprehend <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Comprehend = predicted_conceptual_comprehend,
  Total_Comprehend = predicted_total_comprehend,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_comprehend_long <- melt(plot_data_comprehend, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting Conceptual Comprehension and Total Comprehension
ggplot(plot_data_comprehend_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Conceptual_Comprehend" = "red", "Total_Comprehend" = "blue"),
                     labels = c("Conceptual Comprehension", "Total Comprehension")) +
  scale_linetype_manual(values = c("Mono" = "dotted", "Bil" = "solid")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Conceptual and Total Comprehension") +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2)) +
  theme_minimal()

