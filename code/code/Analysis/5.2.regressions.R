# Clear previous data
rm(list = ls())

# Load data for T1
bil_both_counts_T1 <- read.csv(file.path('raw-csv', 'Analysis', 'bil_both_counts_T1.csv'), header=TRUE, stringsAsFactors=FALSE)
mono_both_counts_T1 <- read.csv(file.path('raw-csv', 'Analysis', 'mono_both_counts_T1.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load data for T2
bil_both_counts_T2 <- read.csv(file.path('raw-csv', 'Analysis', 'bil_both_counts_T2.csv'), header=TRUE, stringsAsFactors=FALSE)
mono_both_counts_T2 <- read.csv(file.path('raw-csv', 'Analysis', 'mono_both_counts_T2.csv'), header=TRUE, stringsAsFactors=FALSE)

# Load summary data with age_CDI 
summary_dat <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA.csv'), header=TRUE, stringsAsFactors=FALSE)


#### mono age 

# Match age_CDI for T1 (token = 1)
mono_both_counts_T1 <- merge(mono_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

# Match age_CDI for T2 (token = 2)
mono_both_counts_T2 <- merge(mono_both_counts_T2, summary_dat[summary_dat$token == 2, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

##### bil age
# Match age_CDI for T1 (token = 1)
bil_both_counts_T1 <- merge(bil_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

# Match age_CDI for T2 (token = 2)
bil_both_counts_T2 <- merge(bil_both_counts_T2, summary_dat[summary_dat$token == 2, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)


#### merged T1 mono/bil 
# Merge mono_both_counts_T1 and bil_both_counts_T1
both_counts_T1 <- rbind(mono_both_counts_T1, bil_both_counts_T1)

#### merged T2 mono/bil
# Merge mono_both_counts_T1 and bil_both_counts_T1
both_counts_T2 <- rbind(mono_both_counts_T2, bil_both_counts_T2)


# Load necessary library
library(interactions)
library(ggplot2)


##### REGRESIONS _____________________________________________
####### TIME POINT 1
###### RQ 1 analysis #######
#L1 Comprehension ---

### main effects-
# Define the regression formula for T1
formula_R1.1C_T1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for T1
mc1.1_T1 <- lm(formula_R1.1C_T1, data = both_counts_T1)

# Summarize the model for T1
summary(mc1.1_T1)


### interaction-
# Define the regression formula for T1
formula_R1.2C_T1 <- L1_comprehension ~ AWC_COUNT_Avg * Mono_Bil + age_CDI

# Run the multiple linear regression for T1
mc1.2_T1 <- lm(formula_R1.2C_T1, data = both_counts_T1)

# Summarize the model for T1
summary(mc1.2_T1)


# ANOVA --
anova_result <- anova(mc1.1_T1, mc1.2_T1)
print(anova_result)

#mc1.2_T1 = better predictor (interaction)


# Create the interaction plot
interact_plot(mc1.2_T1, pred = AWC_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)



#L2 Comprehension ---

# Define the regression formula for L2 comprehension (bil counts T1)
formula_L2_T1 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension (bil counts T1)
model_L2_T1 <- lm(formula_L2_T1, data = bil_both_counts_T1)

# Summarize the model for L2 comprehension (bil counts T1)
summary(model_L2_T1)


# Create the plot
ggplot(bil_both_counts_T1, aes(x = AWC_COUNT_Avg, y = L2_comprehension)) +
  geom_point(aes(color = age_CDI)) +       # Scatter plot colored by age_CDI
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(title = "L2 Comprehension vs. AWC_COUNT_Avg for Bilinguals",
       x = "Average Adult Word Count (AWC_COUNT_Avg)",
       y = "L2 Comprehension") +
  theme_minimal()

#L1 Production ---
#main effects -
# Define the formula for L1 production
formula_R1.1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
mp1.1_T1 <- lm(formula_R1.1_prod, data = both_counts_T1)

# Summarize the results
summary(mp1.1_T1)

#interaction - 
# Define the formula for L1 production
formula_R1.2_prod <- L1_production ~ AWC_COUNT_Avg * Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
mp1.2_T1 <- lm(formula_R1.2_prod, data = both_counts_T1)

# Summarize the results
summary(mp1.2_T1)


# ANOVA --
anova_result <- anova(mp1.1_T1, mp1.2_T1)
print(anova_result)

#mp1.2_T1 = better predictor (interaction)

interact_plot(mp1.2_T1, pred = AWC_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 Production ---

# Define the formula for L2 production regression
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 production on bilingual data
model_L2_prod <- lm(formula_L2_prod, data = bil_both_counts_T1)

# Print the summary of the L2 production regression model
summary(model_L2_prod)

# Create the plot for L2 production
ggplot(bil_both_counts_T1, aes(x = AWC_COUNT_Avg, y = L2_production)) +
  geom_point(aes(color = age_CDI)) +       # Scatter plot colored by age_CDI
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(title = "L2 Production vs. AWC_COUNT_Avg for Bilinguals",
       x = "Average Adult Word Count (AWC_COUNT_Avg)",
       y = "L2 Production") +
  theme_minimal()

########## RQ 2 + CT (BEYOND AWC)----------------------
#L1 comprehension ---

# main effects - 

# Define the regression formula for T1
formula_R2.1C_T1 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg

# Run the multiple linear regression for T1
mc2.1_T1 <- lm(formula_R2.1C_T1, data = both_counts_T1)

# Summarize the model for T1
summary(mc2.1_T1)

# ANOVA --
anova_result <- anova(mc1.2_T1, mc2.1_T1)
print(anova_result)
#mc2.2_T1 = better predictor (interaction)

# interaction - 

# Define the regression formula for T1
formula_R2.2C_T1 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil

# Run the multiple linear regression for T1
mc2.2_T1 <- lm(formula_R2.2C_T1, data = both_counts_T1)

# Summarize the model for T1
summary(mc2.2_T1)


# ANOVA --
anova_result <- anova(mc2.1_T1, mc2.2_T1)
print(anova_result)
#mc2.2_T1 = better predictor (interaction)


# plots 
interact_plot(mc2.2_T1, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 comprehension ---

# Define the formula for L2 comprehension regression
formula_L2_comp <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension on bilingual data
model_L2_comp <- lm(formula_L2_comp, data = bil_both_counts_T1)

# Print the summary of the L2 comprehension regression model
summary(model_L2_comp)


# Plot the effect of CT_COUNT_Avg on L2_comprehension
ggplot(bil_both_counts_T1, aes(x = CT_COUNT_Avg, y = L2_comprehension)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of CT_COUNT_Avg on L2_comprehension",
       x = "CT_COUNT_Avg",
       y = "L2_comprehension") +
  theme_minimal()


#L1 production ---

#main effects -

# Define the formula for L1 production
formula_R2.1_prod <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg 

# Run the multiple linear regression for L1 production
mp2.1_T1 <- lm(formula_R2.1_prod, data = both_counts_T1)

# Summarize the results
summary(mp2.1_T1)


#interaction - 

# Define the formula for L1 production
formula_R2.2_prod <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil

# Run the multiple linear regression for L1 production
mp2.2_T1 <- lm(formula_R2.2_prod, data = both_counts_T1)

# Summarize the results
summary(mp2.2_T1)

# ANOVA --
anova_result <- anova(mp2.1_T1, mp2.2_T1)
print(anova_result)
#mp2.2_T1 = better predictor (interaction)


# plots 
interact_plot(mp2.2_T1, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 production ---
# Define the formula for L2 production regression
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

# Run the multiple linear regression for L1 production
model_L2_prod <- lm(formula_L2_prod, data = bil_both_counts_T1)

# Add residuals to the data frame bil_both_counts_T1
bil_both_counts_T1$residuals_L2_prod <- residuals(model_L2_prod)

# Print the summary of the L2 production regression model
summary(model_L2_prod)

# Plot the effect of CT_COUNT_Avg on L2_comprehension
ggplot(bil_both_counts_T1, aes(x = CT_COUNT_Avg, y = L2_production)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of CT_COUNT_Avg on L2_production",
       x = "CT_COUNT_Avg",
       y = "L2_production") +
  theme_minimal()

##### RQ 3 ------------------

# Conceptual Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_conceptual_comp <- L1_conceptual_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comp <- lm(formula_conceptual_comp, data = bil_both_counts_T1)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comp)



# Conceptual Production ----

# Define the formula for conceptual vocab production
formula_conceptual_prod <- L1_conceptual_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod <- lm(formula_conceptual_prod, data = bil_both_counts_T1)

# Add residuals to the data frame bil_both_counts_T1
bil_both_counts_T1$residuals_conceptual_prod <- residuals(model_conceptual_prod)

# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod)


# Total Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_total_comp <- L1_total_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab comprehension
model_total_comp <- lm(formula_total_comp, data = bil_both_counts_T1)

# Add residuals to the data frame bil_both_counts_T1
bil_both_counts_T1$residuals_total_comp <- residuals(model_total_comp)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_total_comp)


# Total Production ----

# Define the formula for conceptual vocab production
formula_total_prod <- L1_total_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab production
model_total_prod <- lm(formula_total_prod, data = bil_both_counts_T1)

# Add residuals to the data frame bil_both_counts_T1
bil_both_counts_T1$residuals_total_prod <- residuals(model_total_prod)

# Print the summary of the conceptual vocab production regression model
summary(model_total_prod)



####### TIME POINT 2
###### RQ 1 analysis #######
#L1 Comprehension ---

### main effects-
# Define the regression formula for T2
formula_R1.1C_T2 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for T2
mc1.1_T2 <- lm(formula_R1.1C_T2, data = both_counts_T2)

# Summarize the model for T2
summary(mc1.1_T2)


### interaction-
# Define the regression formula for T2
formula_R1.2C_T2 <- L1_comprehension ~ AWC_COUNT_Avg * Mono_Bil + age_CDI

# Run the multiple linear regression for T2
mc1.2_T2 <- lm(formula_R1.2C_T2, data = both_counts_T2)

# Summarize the model for T2
summary(mc1.2_T2)


# ANOVA --
anova_result <- anova(mc1.1_T2, mc1.2_T2)
print(anova_result)

#mc1.2_T2 = better predictor (interaction)


# Create the interaction plot
interact_plot(mc1.2_T2, pred = AWC_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)



#L2 Comprehension ---

# Define the regression formula for L2 comprehension (bil counts T2)
formula_L2_T2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension (bil counts T2)
model_L2_T2 <- lm(formula_L2_T2, data = bil_both_counts_T2)

# Summarize the model for L2 comprehension (bil counts T2)
summary(model_L2_T2)


# Create the plot
ggplot(bil_both_counts_T2, aes(x = AWC_COUNT_Avg, y = L2_comprehension)) +
  geom_point(aes(color = age_CDI)) +       # Scatter plot colored by age_CDI
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(title = "L2 Comprehension vs. AWC_COUNT_Avg for Bilinguals",
       x = "Average Adult Word Count (AWC_COUNT_Avg)",
       y = "L2 Comprehension") +
  theme_minimal()

#L1 Production ---
#main effects -
# Define the formula for L1 production
formula_R1.1_prod_T2 <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
mp1.1_T2 <- lm(formula_R1.1_prod_T2, data = both_counts_T2)

# Summarize the results
summary(mp1.1_T2)

#interaction - 
# Define the formula for L1 production
formula_R1.2_prod_T2 <- L1_production ~ AWC_COUNT_Avg * Mono_Bil + age_CDI

# Run the multiple linear regression for L1 production
mp1.2_T2 <- lm(formula_R1.2_prod_T2, data = both_counts_T2)

# Summarize the results
summary(mp1.2_T2)


# ANOVA --
anova_result <- anova(mp1.1_T2, mp1.2_T2)
print(anova_result)

#mp1.2_T1 = better predictor (interaction)

interact_plot(mp1.2_T2, pred = AWC_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 Production ---

# Define the formula for L2 production regression
formula_L2_prod_T2 <- L2_production ~ AWC_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 production on bilingual data
model_L2_prod_T2 <- lm(formula_L2_prod_T2, data = bil_both_counts_T2)

# Print the summary of the L2 production regression model
summary(model_L2_prod_T2)

# Create the plot for L2 production
ggplot(bil_both_counts_T2, aes(x = AWC_COUNT_Avg, y = L2_production)) +
  geom_point(aes(color = age_CDI)) +       # Scatter plot colored by age_CDI
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(title = "L2 Production vs. AWC_COUNT_Avg for Bilinguals",
       x = "Average Adult Word Count (AWC_COUNT_Avg)",
       y = "L2 Production") +
  theme_minimal()

########## RQ 2 + CT (BEYOND AWC)----------------------
#L1 comprehension ---

# main effects - 

# Define the regression formula for T2
formula_R2.1C_T2 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg

# Run the multiple linear regression for T2
mc2.1_T2 <- lm(formula_R2.1C_T2, data = both_counts_T2)

# Summarize the model for T2
summary(mc2.1_T2)

# ANOVA --
anova_result <- anova(mc1.2_T2, mc2.1_T2)
print(anova_result)
#mc2.2_T1 = better predictor (interaction)

# interaction - 

# Define the regression formula for T1
formula_R2.2C_T2 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil

# Run the multiple linear regression for T1
mc2.2_T2 <- lm(formula_R2.2C_T1, data = both_counts_T2)

# Summarize the model for T1
summary(mc2.2_T2)


# ANOVA --
anova_result <- anova(mc2.1_T2, mc2.2_T2)
print(anova_result)
#mc2.2_T2 = better predictor (interaction)


# plots 
interact_plot(mc2.2_T2, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)

#L2 comprehension ---

# Define the formula for L2 comprehension regression
formula_L2_comp_T2 <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

# Run the multiple linear regression for L2 comprehension on bilingual data
model_L2_comp_T2 <- lm(formula_L2_comp_T2, data = bil_both_counts_T2)

# Print the summary of the L2 comprehension regression model
summary(model_L2_comp_T2)


# Plot the effect of CT_COUNT_Avg on L2_comprehension
ggplot(bil_both_counts_T2, aes(x = CT_COUNT_Avg, y = L2_comprehension)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of CT_COUNT_Avg on L2_comprehension",
       x = "CT_COUNT_Avg",
       y = "L2_comprehension") +
  theme_minimal()



#L1 production ---

#main effects -

# Define the formula for L1 production
formula_R2.1_prod_T2 <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg 

# Run the multiple linear regression for L1 production
mp2.1_T2 <- lm(formula_R2.1_prod_T2, data = both_counts_T2)

# Summarize the results
summary(mp2.1_T2)


#interaction - 

# Define the formula for L1 production
formula_R2.2_prod_T2 <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil

# Run the multiple linear regression for L1 production
mp2.2_T2 <- lm(formula_R2.2_prod_T2, data = both_counts_T2)

# Summarize the results
summary(mp2.2_T2)

# ANOVA --
anova_result <- anova(mp2.1_T2, mp2.2_T2)
print(anova_result)
#mp2.2_T2 = better predictor (interaction)

# plots 
interact_plot(mp2.2_T2, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 production ---
# Define the formula for L2 production regression
formula_L2_prod_T2 <- L2_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI

# Run the multiple linear regression for L1 production
model_L2_prod_T2 <- lm(formula_L2_prod_T2, data = bil_both_counts_T2)

# Print the summary of the L2 production regression model
summary(model_L2_prod_T2)

# Plot the effect of CT_COUNT_Avg on L2_comprehension
ggplot(bil_both_counts_T2, aes(x = CT_COUNT_Avg, y = L2_production)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Effect of CT_COUNT_Avg on L2_production",
       x = "CT_COUNT_Avg",
       y = "L2_production") +
  theme_minimal()

##### RQ 3 ------------------

# Conceptual Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_conceptual_comp_T2<- L1_conceptual_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comp_T2 <- lm(formula_conceptual_comp_T2, data = bil_both_counts_T2)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comp_T2)



# Conceptual Production ----

# Define the formula for conceptual vocab production
formula_conceptual_prod_T2 <- L1_conceptual_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod_T2 <- lm(formula_conceptual_prod_T2, data = bil_both_counts_T2)

# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod_T2)


# Total Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_total_comp_T2 <- L1_total_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab comprehension
model_total_comp_T2 <- lm(formula_total_comp_T2, data = bil_both_counts_T2)

# Print the summary of the conceptual vocab comprehension regression model
summary(model_total_comp_T2)


# Total Production ----

# Define the formula for conceptual vocab production
formula_total_prod_T2 <- L1_total_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg

# Run the multiple linear regression for conceptual vocab production
model_total_prod_T2 <- lm(formula_total_prod_T2, data = bil_both_counts_T2)

# Print the summary of the conceptual vocab production regression model
summary(model_total_prod_T2)

