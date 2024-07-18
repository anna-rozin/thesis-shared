# Clear previous data
rm(list = ls())

# Load the data T1
both_counts <- read.csv(file.path('raw-csv', 'Analysis', '9.both_counts_L1L2.csv'), header=TRUE, stringsAsFactors=FALSE)
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


##### L1 comprehension
# Define the formula for L1 comprehension regression
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 comprehension
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Print the summary of the L1 comprehension regression model
summary(model_L1)

##### L2 comprehension
# Define the formula for L2 comprehension regression
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension
model_L2 <- lm(formula_L2, data = merged_both_counts)

# Print the summary of the L2 comprehension regression model
summary(model_L2)


##### L1 production
# Define the formula for L1 production
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# Summarize the results
summary(model_L1_prod)

#### L2 production
# Define the formula for L2 production
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L2 production
model_L2_prod <- lm(formula_L2_prod, data = merged_both_counts)

# Summarize the results
summary(model_L2_prod)


###### RQ 2 analysis #######

##### L1 comprehension
# Define the formula for L1 comprehension regression
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 comprehension
model_L1 <- lm(formula_L1, data = merged_both_counts)

# Print the summary of the L1 comprehension regression model
summary(model_L1)

##### L2 comprehension
# Define the formula for L2 comprehension regression
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L2 comprehension
model_L2 <- lm(formula_L2, data = merged_both_counts)

# Print the summary of the L2 comprehension regression model
summary(model_L2)


##### L1 production
# Define the formula for L1 production
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# Summarize the results
summary(model_L1_prod)

#### L2 production


# Subset the data to include only bilingual participants
bilingual_data <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formula for L2 production regression
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

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


#### RQ 3 Analyis 

# Conceptual Comprehension
# Define the formula for conceptual vocab comprehension
formula_conceptual_comprehend <- L1_conceptual_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comprehend)


### Conceptual Production
# Define the formula for conceptual vocab production
formula_conceptual_prod <- L1_conceptual_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod <- lm(formula_conceptual_prod, data = merged_both_counts)

# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod)

#Total Comprehension
# Define the formula for total vocab comprehension
formula_total_comprehend <- L1_total_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for total vocab comprehension
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)

# Print the summary of the total vocab comprehension regression model
summary(model_total_comprehend)

#Total Production
# Define the formula for total vocab production
formula_total_prod <- L1_total_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for total vocab production
model_total_prod <- lm(formula_total_prod, data = merged_both_counts)

# Print the summary of the total vocab production regression model
summary(model_total_prod)



#### LENA GRAPH 
# Calculate counts of participants in each AWC_COUNT_Avg range
summary_counts <- data.frame(
  Range = cut(merged_both_counts$AWC_COUNT_Avg, breaks = seq(0, 13000, by = 1000), right = FALSE),
  Participants = as.vector(table(cut(merged_both_counts$AWC_COUNT_Avg, breaks = seq(0, 13000, by = 1000), right = FALSE)))
)

# Plotting using ggplot2
library(ggplot2)

# Create a bar plot
ggplot(summary_counts, aes(x = as.factor(Range), y = Participants)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "AWC_COUNT_Avg Range", y = "Number of Participants") +
  ggtitle("Number of Participants by AWC_COUNT_Avg Range") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) gsub("\\.", "-", x))  # Adjust x-axis labels if needed


#############
library(ggplot2)

# Extract predicted values from the models
predicted_L1_comprehend <- predict(model_L1, newdata = merged_both_counts)
predicted_L2_comprehend <- predict(model_L2, newdata = merged_both_counts)
predicted_L1_prod <- predict(model_L1_prod, newdata = merged_both_counts)
predicted_L2_prod <- predict(model_L2_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  L1_comprehend = predicted_L1_comprehend,
  L2_comprehend = predicted_L2_comprehend,
  L1_prod = predicted_L1_prod,
  L2_prod = predicted_L2_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Plotting L1 comprehension
ggplot(plot_data, aes(x = age_CDI, y = L1_comprehend, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted L1 Comprehension", color = "Mono/Bil") +
  ggtitle("Predicted L1 Comprehension by Age and Group") +
  theme_minimal()

# Plotting L2 comprehension
ggplot(plot_data, aes(x = age_CDI, y = L2_comprehend, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted L2 Comprehension", color = "Mono/Bil") +
  ggtitle("Predicted L2 Comprehension by Age and Group") +
  theme_minimal()

# Plotting L1 production
ggplot(plot_data, aes(x = age_CDI, y = L1_prod, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted L1 Production", color = "Mono/Bil") +
  ggtitle("Predicted L1 Production by Age and Group") +
  theme_minimal()

# Plotting L2 production
ggplot(plot_data, aes(x = age_CDI, y = L2_prod, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted L2 Production", color = "Mono/Bil") +
  ggtitle("Predicted L2 Production by Age and Group") +
  theme_minimal()




################

# Extract predicted values from the models
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_conceptual_prod <- predict(model_conceptual_prod, newdata = merged_both_counts)
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)
predicted_total_prod <- predict(model_total_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Conceptual_Comprehension = predicted_conceptual_comprehend,
  Conceptual_Production = predicted_conceptual_prod,
  Total_Comprehension = predicted_total_comprehend,
  Total_Production = predicted_total_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Plotting Conceptual Comprehension
ggplot(plot_data, aes(x = age_CDI, y = Conceptual_Comprehension, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted Conceptual Comprehension", color = "Mono/Bil") +
  ggtitle("Predicted Conceptual Comprehension by Age and Group") +
  theme_minimal()

# Plotting Conceptual Production
ggplot(plot_data, aes(x = age_CDI, y = Conceptual_Production, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted Conceptual Production", color = "Mono/Bil") +
  ggtitle("Predicted Conceptual Production by Age and Group") +
  theme_minimal()

# Plotting Total Comprehension
ggplot(plot_data, aes(x = age_CDI, y = Total_Comprehension, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted Total Comprehension", color = "Mono/Bil") +
  ggtitle("Predicted Total Comprehension by Age and Group") +
  theme_minimal()

# Plotting Total Production
ggplot(plot_data, aes(x = age_CDI, y = Total_Production, color = Mono_Bil)) +
  geom_line(aes(group = Mono_Bil), size = 1) +
  labs(x = "Age (CDI)", y = "Predicted Total Production", color = "Mono/Bil") +
  ggtitle("Predicted Total Production by Age and Group") +
  theme_minimal()



######## TRYING TO MERGE THE PLOTS 
library(ggplot2)
library(reshape2)

# Define the formulas and run the multiple linear regressions
# L1 comprehension
formula_L1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L1 <- lm(formula_L1, data = merged_both_counts)

# L2 comprehension
formula_L2 <- L2_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L2 <- lm(formula_L2, data = merged_both_counts)

# Extract predicted values from the models
predicted_L1 <- predict(model_L1, newdata = merged_both_counts)
predicted_L2 <- predict(model_L2, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  L1_comprehension = predicted_L1,
  L2_comprehension = predicted_L2,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_long <- melt(plot_data, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Comprehension_Type", value.name = "Comprehension")

# Plotting L1 and L2 comprehension
ggplot(plot_data_long, aes(x = age_CDI, y = Comprehension, color = Comprehension_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("L1_comprehension" = "red", "L2_comprehension" = "blue"),
                     labels = c("L1", "L2")) +
  scale_linetype_manual(values = c("Mono" = "solid", "Bil" = "dotted")) +
  labs(x = "Age (CDI)", y = "Comprehension", color = "Comprehension Type", linetype = "Mono/Bil") +
  ggtitle("Comprehension") +
  theme_minimal()


#### Production

library(ggplot2)
library(reshape2)

# Define the formulas and run the multiple linear regressions
# L1 production
formula_L1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L1_prod <- lm(formula_L1_prod, data = merged_both_counts)

# L2 production
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
model_L2_prod <- lm(formula_L2_prod, data = merged_both_counts)

# Extract predicted values from the models
predicted_L1_prod <- predict(model_L1_prod, newdata = merged_both_counts)
predicted_L2_prod <- predict(model_L2_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_prod <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  L1_production = predicted_L1_prod,
  L2_production = predicted_L2_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data frame to long format for easier plotting with ggplot2
plot_data_prod_long <- melt(plot_data_prod, id.vars = c("age_CDI", "Mono_Bil"), variable.name = "Production_Type", value.name = "Production")

# Plotting L1 and L2 production
ggplot(plot_data_prod_long, aes(x = age_CDI, y = Production, color = Production_Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("L1_production" = "red", "L2_production" = "blue"),
                     labels = c("L1", "L2")) +
  scale_linetype_manual(values = c("Mono" = "solid", "Bil" = "dotted")) +
  labs(x = "Age (CDI)", y = "Production", color = "Production Type", linetype = "Mono/Bil") +
  ggtitle("Production") +
  theme_minimal()

#### conceptual 
# Load necessary libraries
library(ggplot2)

# Define the formulas for conceptual comprehension and production
formula_conceptual_comprehend <- L1_conceptual_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
formula_conceptual_prod <- L1_conceptual_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regressions
model_conceptual_comprehend <- lm(formula_conceptual_comprehend, data = merged_both_counts)
model_conceptual_prod <- lm(formula_conceptual_prod, data = merged_both_counts)

# Predict values for conceptual comprehension and production
predicted_conceptual_comprehend <- predict(model_conceptual_comprehend, newdata = merged_both_counts)
predicted_conceptual_prod <- predict(model_conceptual_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_conceptual <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Comprehension = predicted_conceptual_comprehend,
  Production = predicted_conceptual_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data for ggplot
plot_data_conceptual_long <- tidyr::gather(plot_data_conceptual, Type, Value, Comprehension, Production)

# Plotting conceptual comprehension and production
ggplot(plot_data_conceptual_long, aes(x = age_CDI, y = Value, color = Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  labs(x = "Age (CDI)", y = "Conceptual Score", color = "Type", linetype = "Group") +
  ggtitle("Conceptual Comprehension and Production") +
  theme_minimal()


##### total 
# Define the formulas for total vocab comprehension and production
formula_total_comprehend <- L1_total_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI
formula_total_prod <- L1_total_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regressions
model_total_comprehend <- lm(formula_total_comprehend, data = merged_both_counts)
model_total_prod <- lm(formula_total_prod, data = merged_both_counts)

# Predict values for total vocab comprehension and production
predicted_total_comprehend <- predict(model_total_comprehend, newdata = merged_both_counts)
predicted_total_prod <- predict(model_total_prod, newdata = merged_both_counts)

# Create a data frame for plotting
plot_data_total <- data.frame(
  age_CDI = merged_both_counts$age_CDI,
  Comprehension = predicted_total_comprehend,
  Production = predicted_total_prod,
  Mono_Bil = merged_both_counts$Mono_Bil
)

# Reshape the data for ggplot
plot_data_total_long <- tidyr::gather(plot_data_total, Type, Value, Comprehension, Production)

# Plotting total vocab comprehension and production
ggplot(plot_data_total_long, aes(x = age_CDI, y = Value, color = Type, linetype = Mono_Bil)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  labs(x = "Age (CDI)", y = "Total Vocabulary Score", color = "Type", linetype = "Group") +
  ggtitle("Total Vocabulary Comprehension and Production") +
  theme_minimal()



### TOTAL

library(ggplot2)
library(reshape2)

# Subset the data to include only bilingual participants for L2 comprehension and production
merged_both_counts_bil <- subset(merged_both_counts, Mono_Bil == "Bil")

# Define the formulas and run the multiple linear regressions
# Total Comprehension
formula_comprehend <- skill_total_comprehend ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
model_comprehend <- lm(formula_comprehend, data = merged_both_counts)

# Total Production
formula_produce <- skill_total_produce ~ AWC_COUNT_Avg * Mono_Bil + age_CDI
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
  ggtitle("Total Comprehension and Production") +
  theme_minimal()


