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
summary_dat_T2 <- read.csv(file.path('raw-csv', 'Summary', '8.merged_cleaned_CDI_LENA_T2.csv'), header=TRUE, stringsAsFactors=FALSE)


#### mono age 

# Match age_CDI for T1 (token = 1)
mono_both_counts_T1 <- merge(mono_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

# Match age_CDI for T2 (token = 2)
mono_both_counts_T2 <- merge(mono_both_counts_T2, summary_dat_T2[summary_dat_T2$token == 2, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

##### bil age
# Match age_CDI for T1 (token = 1)
bil_both_counts_T1 <- merge(bil_both_counts_T1, summary_dat[summary_dat$token == 1, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)

# Match age_CDI for T2 (token = 2)
bil_both_counts_T2 <- merge(bil_both_counts_T2, summary_dat_T2[summary_dat_T2$token == 2, c("ID_lab", "age_CDI")], by = "ID_lab", all.x = TRUE)


#### merged T1 mono/bil 
# Merge mono_both_counts_T1 and bil_both_counts_T1
both_counts_T1 <- rbind(mono_both_counts_T1, bil_both_counts_T1)

#### merged T2 mono/bil
# Merge mono_both_counts_T1 and bil_both_counts_T1
both_counts_T2 <- rbind(mono_both_counts_T2, bil_both_counts_T2)


#removing ALL duplicates 
bil_both_counts_T1 <- unique(bil_both_counts_T1)
bil_both_counts_T2 <- unique(bil_both_counts_T2)
both_counts_T1 <- unique(both_counts_T1)
both_counts_T2 <- unique(both_counts_T2)
mono_both_counts_T1 <- unique(mono_both_counts_T1)
mono_both_counts_T2 <- unique(mono_both_counts_T2)
summary_dat <- unique(summary_dat)


# Load necessary library
library(interactions)
library(ggplot2)


##### REGRESIONS _____________________________________________
####### TIME POINT 1
###### RQ 1 analysis #######

#L1 Comprehension ---

### main effects-
# Define the regression formula for T1
formula_R1.1C_T1 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for T1
mc1.1_T1 <- lm(formula_R1.1C_T1, data = both_counts_T1)
# Summarize the model for T1
summary(mc1.1_T1)


### interaction-
# Define the regression formula for T1
formula_R1.2C_T1 <- L1_comprehension ~ AWC_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg 
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
formula_L2_T1 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R1.1_prod <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.1_T1 <- lm(formula_R1.1_prod, data = both_counts_T1)
# Summarize the results
summary(mp1.1_T1)

#interaction - 
# Define the formula for L1 production
formula_R1.2_prod <- L1_production ~ AWC_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg
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
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R2.1C_T1 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
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
formula_R2.2C_T1 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
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
formula_L2_comp <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R2.1_prod <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp2.1_T1 <- lm(formula_R2.1_prod, data = both_counts_T1)
# Summarize the results
summary(mp2.1_T1)


#interaction - 
# Define the formula for L1 production
formula_R2.2_prod <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
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
formula_L2_prod <- L2_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
model_L2_prod <- lm(formula_L2_prod, data = bil_both_counts_T1)
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


###### RQ 2 + CT (BEYOND AWC) for child vocalizations (CV)---- 

# L1 comprehension main effects----

# Define the formula for L1 comprehension with main effects
formula_L1_CV_comp_main <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 comprehension
model_L1_CV_comp_main <- lm(formula_L1_CV_comp_main, data = both_counts_T1)
# Summarize the model for L1 comprehension
summary(model_L1_CV_comp_main)

# L1 comprehension interaction----

# Define the formula for L1 comprehension with interaction effects
formula_L1_CV_comp_interact <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
# Run the multiple linear regression for L1 comprehension
model_L1_CV_comp_interact <- lm(formula_L1_CV_comp_interact, data = both_counts_T1)
# Summarize the model for L1 comprehension
summary(model_L1_CV_comp_interact)

# Compare the models using ANOVA
anova_result_L1_CV_comp <- anova(model_L1_CV_comp_main, model_L1_CV_comp_interact)
print(anova_result_L1_CV_comp)
# interaction is marginally significant 

#L2 comprehension --- main effects

# Define the formula for L2 comprehension
formula_L2_CV_comp <- CV_COUNT_Avg ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 comprehension
model_L2_CV_comp <- lm(formula_L2_CV_comp, data = bil_both_counts_T1)
# Print the summary of the L2 comprehension regression model
summary(model_L2_CV_comp)


#L1 production--- main effects 

# Define the formula for L1 production with main effects
formula_L1_CV_prod_main <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
model_L1_CV_prod_main <- lm(formula_L1_CV_prod_main, data = both_counts_T1)
# Summarize the results
summary(model_L1_CV_prod_main)


#L1 production --- interaction 

# Define the formula for L1 production with interaction effects
formula_L1_CV_prod_interact <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
model_L1_CV_prod_interact <- lm(formula_L1_CV_prod_interact, data = both_counts_T1)
# Summarize the results
summary(model_L1_CV_prod_interact)

# Compare the models using ANOVA
anova_result_L1_CV_prod <- anova(model_L1_CV_prod_main, model_L1_CV_prod_interact)
print(anova_result_L1_CV_prod)

# Plot interaction effects for L1 production
interact_plot(model_L1_CV_prod_interact, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)


#L2 production
# Define the formula for L2 production
formula_L2_CV_prod <- CV_COUNT_Avg ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 production
model_L2_CV_prod <- lm(formula_L2_CV_prod, data = bil_both_counts_T1)
# Print the summary of the L2 production regression model
summary(model_L2_CV_prod)


##### RQ 3 ------------------

# Conceptual Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_conceptual_comp <- L1_conceptual_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg + Mono_Bil 
# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comp <- lm(formula_conceptual_comp, data = both_counts_T1)
# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comp)


# Conceptual Production ----

# Define the formula for conceptual vocab production
formula_conceptual_prod <- L1_conceptual_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Mono_Bil
# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod <- lm(formula_conceptual_prod, data = both_counts_T1)
# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod)


# Total Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_total_comp <- L1_total_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg + Mono_Bil
# Run the multiple linear regression for conceptual vocab comprehension
model_total_comp <- lm(formula_total_comp, data = both_counts_T1)
# Print the summary of the conceptual vocab comprehension regression model
summary(model_total_comp)


# Total Production ----

# Define the formula for conceptual vocab production
formula_total_prod <- L1_total_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg + Mono_Bil
# Run the multiple linear regression for conceptual vocab production
model_total_prod <- lm(formula_total_prod, data = both_counts_T1)
# Print the summary of the conceptual vocab production regression model
summary(model_total_prod)


####### TIME POINT 2
###### RQ 1 analysis #######
#L1 Comprehension ---

### main effects-
# Define the regression formula for T2
formula_R1.1C_T2 <- L1_comprehension ~ AWC_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for T2
mc1.1_T2 <- lm(formula_R1.1C_T2, data = both_counts_T2)
# Summarize the model for T2
summary(mc1.1_T2)


### interaction-
# Define the regression formula for T2
formula_R1.2C_T2 <- L1_comprehension ~ AWC_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg
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
formula_L2_T2 <- L2_comprehension ~ AWC_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R1.1_prod_T2 <- L1_production ~ AWC_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.1_T2 <- lm(formula_R1.1_prod_T2, data = both_counts_T2)
# Summarize the results
summary(mp1.1_T2)

#interaction - 
# Define the formula for L1 production
formula_R1.2_prod_T2 <- L1_production ~ AWC_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg
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
formula_L2_prod_T2 <- L2_production ~ AWC_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R2.1C_T2 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
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
formula_R2.2C_T2 <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
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
formula_L2_comp_T2 <- L2_comprehension ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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
formula_R2.1_prod_T2 <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp2.1_T2 <- lm(formula_R2.1_prod_T2, data = both_counts_T2)
# Summarize the results
summary(mp2.1_T2)


#interaction - 

# Define the formula for L1 production
formula_R2.2_prod_T2 <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
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
formula_L2_prod_T2 <- L2_production ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
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


###### RQ 2 + CT (BEYOND AWC) for child vocalizations (CV)---- 

# L1 comprehension main effects----

# Define the formula for L1 comprehension with main effects
formula_L1_CV_comp_main_T2 <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 comprehension
model_L1_CV_comp_main_T2 <- lm(formula_L1_CV_comp_main_T2, data = both_counts_T2)
# Summarize the model for L1 comprehension
summary(model_L1_CV_comp_main_T2)

# L1 comprehension interaction----

# Define the formula for L1 comprehension with interaction effects
formula_L1_CV_comp_interact_T2 <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
# Run the multiple linear regression for L1 comprehension
model_L1_CV_comp_interact_T2 <- lm(formula_L1_CV_comp_interact_T2, data = both_counts_T2)
# Summarize the model for L1 comprehension
summary(model_L1_CV_comp_interact_T2)

# Compare the models using ANOVA
anova_result_L1_CV_comp_T2 <- anova(model_L1_CV_comp_main_T2, model_L1_CV_comp_interact_T2)
print(anova_result_L1_CV_comp_T2)
# interaction is marginally significant 

#L2 comprehension --- main effects

# Define the formula for L2 comprehension
formula_L2_CV_comp_T2 <- CV_COUNT_Avg ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 comprehension
model_L2_CV_comp_T2 <- lm(formula_L2_CV_comp_T2, data = bil_both_counts_T2)
# Print the summary of the L2 comprehension regression model
summary(model_L2_CV_comp_T2)

#L1 production--- main effects 

# Define the formula for L1 production with main effects
formula_L1_CV_prod_main_T2 <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
model_L1_CV_prod_main_T2 <- lm(formula_L1_CV_prod_main_T2, data = both_counts_T2)
# Summarize the results
summary(model_L1_CV_prod_main_T2)

#L1 production --- interaction 

# Define the formula for L1 production with interaction effects
formula_L1_CV_prod_interact_T2 <- CV_COUNT_Avg ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg * Mono_Bil + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
model_L1_CV_prod_interact_T2 <- lm(formula_L1_CV_prod_interact_T2, data = both_counts_T2)
# Summarize the results
summary(model_L1_CV_prod_interact_T2)

# Compare the models using ANOVA
anova_result_L1_CV_prod_T2 <- anova(model_L1_CV_prod_main_T2, model_L1_CV_prod_interact_T2)
print(anova_result_L1_CV_prod_T2)
# interaction is better predictor 

# Plot interaction effects for L1 production
interact_plot(model_L1_CV_prod_interact_T2, pred = CT_COUNT_Avg, modx = Mono_Bil, plot.points = TRUE)

#L2 production ---

# Define the formula for L2 production
formula_L2_CV_prod_T2 <- CV_COUNT_Avg ~ AWC_COUNT_Avg + CT_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 production
model_L2_CV_prod_T2 <- lm(formula_L2_CV_prod_T2, data = bil_both_counts_T2)
# Print the summary of the L2 production regression model
summary(model_L2_CV_prod_T2)





##### RQ 3 ------------------

# Conceptual Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_conceptual_comp_T2<- L1_conceptual_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for conceptual vocab comprehension
model_conceptual_comp_T2 <- lm(formula_conceptual_comp_T2, data = bil_both_counts_T2)
# Print the summary of the conceptual vocab comprehension regression model
summary(model_conceptual_comp_T2)



# Conceptual Production ----

# Define the formula for conceptual vocab production
formula_conceptual_prod_T2 <- L1_conceptual_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for conceptual vocab production
model_conceptual_prod_T2 <- lm(formula_conceptual_prod_T2, data = bil_both_counts_T2)
# Print the summary of the conceptual vocab production regression model
summary(model_conceptual_prod_T2)


# Total Comprehension ----

# Define the formula for conceptual vocab comprehension
formula_total_comp_T2 <- L1_total_comprehension ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for conceptual vocab comprehension
model_total_comp_T2 <- lm(formula_total_comp_T2, data = bil_both_counts_T2)
# Print the summary of the conceptual vocab comprehension regression model
summary(model_total_comp_T2)


# Total Production ----

# Define the formula for conceptual vocab production
formula_total_prod_T2 <- L1_total_production ~ AWC_COUNT_Avg + age_CDI + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for conceptual vocab production
model_total_prod_T2 <- lm(formula_total_prod_T2, data = bil_both_counts_T2)
# Print the summary of the conceptual vocab production regression model
summary(model_total_prod_T2)






##### Time point 1 testing child vocalizations, first research question ########

#L1 Comprehension
### main effects-
# Define the regression formula for T1
formula_R1.1C_T1_CV <- L1_comprehension ~ CV_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for T1
mc1.1_T1_CV <- lm(formula_R1.1C_T1_CV, data = both_counts_T1)
# Summarize the model for T1
summary(mc1.1_T1_CV)


### interaction-
# Define the regression formula for T1
formula_R1.2C_T1_CV <- L1_comprehension ~ CV_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg 
# Run the multiple linear regression for T1
mc1.2_T1_CV <- lm(formula_R1.2C_T1_CV, data = both_counts_T1)
# Summarize the model for T1
summary(mc1.2_T1_CV)

#L2 Comprehension ---
# Define the regression formula for L2 comprehension (bil counts T1)
formula_L2_T1_CV <- L2_comprehension ~ CV_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 comprehension (bil counts T1)
model_L2_T1_CV <- lm(formula_L2_T1_CV, data = bil_both_counts_T1)
# Summarize the model for L2 comprehension (bil counts T1)
summary(model_L2_T1_CV)

#L1 Production ---

#main effects -
# Define the formula for L1 production
formula_R1.1_prod_CV <- L1_production ~ CV_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.1_T1_CV <- lm(formula_R1.1_prod_CV, data = both_counts_T1)
# Summarize the results
summary(mp1.1_T1_CV)


#interaction - 
# Define the formula for L1 production
formula_R1.2_prod_CV <- L1_production ~ CV_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.2_T1_CV <- lm(formula_R1.2_prod_CV, data = both_counts_T1)
# Summarize the results
summary(mp1.2_T1_CV)

#L2 Production ---

# Define the formula for L2 production regression
formula_L2_prod_CV <- L2_production ~ CV_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 production on bilingual data
model_L2_prod_CV <- lm(formula_L2_prod_CV, data = bil_both_counts_T1)
# Print the summary of the L2 production regression model
summary(model_L2_prod_CV)


##### Time point 2 testing child vocalizations, first research question ########

#L1 Comprehension
### main effects-
# Define the regression formula for T2
formula_R1.1C_T2_CV <- L1_comprehension ~ CV_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for T2
mc1.1_T2_CV <- lm(formula_R1.1C_T2_CV, data = both_counts_T2)
# Summarize the model for T2
summary(mc1.1_T2_CV)


### interaction-
# Define the regression formula for T2
formula_R1.2C_T2_CV <- L1_comprehension ~ CV_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg 
# Run the multiple linear regression for T2
mc1.2_T2_CV <- lm(formula_R1.2C_T2_CV, data = both_counts_T2)
# Summarize the model for T2
summary(mc1.2_T2_CV)

#L2 Comprehension ---
# Define the regression formula for L2 comprehension (bil counts T2)
formula_L2_T2_CV <- L2_comprehension ~ CV_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 comprehension (bil counts T2)
model_L2_T2_CV <- lm(formula_L2_T2_CV, data = bil_both_counts_T2)
# Summarize the model for L2 comprehension (bil counts T2)
summary(model_L2_T2_CV)

#L1 Production ---

#main effects -
# Define the formula for L1 production
formula_R1.1_prod_CV <- L1_production ~ CV_COUNT_Avg + Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.1_T2_CV <- lm(formula_R1.1_prod_CV, data = both_counts_T2)
# Summarize the results
summary(mp1.1_T2_CV)


#interaction - 
# Define the formula for L1 production
formula_R1.2_prod_CV <- L1_production ~ CV_COUNT_Avg * Mono_Bil + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L1 production
mp1.2_T2_CV <- lm(formula_R1.2_prod_CV, data = both_counts_T2)
# Summarize the results
summary(mp1.2_T2_CV)

#L2 Production ---

# Define the formula for L2 production regression
formula_L2_prod_CV <- L2_production ~ CV_COUNT_Avg + age_CDI + Sex + Overlap_Avg
# Run the multiple linear regression for L2 production on bilingual data
model_L2_prod_CV <- lm(formula_L2_prod_CV, data = bil_both_counts_T2)
# Print the summary of the L2 production regression model
summary(model_L2_prod_CV)








############# CDI T1 and T2 testing gain
#  I am not sure if I am doing this correctly......

# Calculate CDI gain
both_counts_T2 <- both_counts_T2 %>%
  left_join(both_counts_T1 %>% select(ID_lab, age_CDI_T1 = age_CDI), by = "ID_lab") %>%
  mutate(CDI_gain = age_CDI - age_CDI_T1)

# L1 Comprehension
# Define the formula for L1 comprehension including CDI gain
formula_L1_CV_comp_gain <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg + CDI_gain
# Run the multiple linear regression for L1 comprehension with CDI gain
model_L1_CV_comp_gain <- lm(formula_L1_CV_comp_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L1_CV_comp_gain)

# Define the original model for L1 comprehension without CDI gain
formula_L1_CV_comp_no_gain <- L1_comprehension ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the model without CDI gain
model_L1_CV_comp_no_gain <- lm(formula_L1_CV_comp_no_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L1_CV_comp_no_gain)

# Compare models using ANOVA for L1 comprehension
anova_result_comp <- anova(model_L1_CV_comp_no_gain, model_L1_CV_comp_gain)
print(anova_result_comp)

# L1 Production
# Define the formula for L1 production including CDI gain
formula_L1_CV_prod_gain <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg + CDI_gain
# Run the multiple linear regression for L1 production with CDI gain
model_L1_CV_prod_gain <- lm(formula_L1_CV_prod_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L1_CV_prod_gain)

# Define the original model for L1 production without CDI gain
formula_L1_CV_prod_no_gain <- L1_production ~ age_CDI + AWC_COUNT_Avg * Mono_Bil + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the model without CDI gain
model_L1_CV_prod_no_gain <- lm(formula_L1_CV_prod_no_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L1_CV_prod_no_gain)

# Compare models using ANOVA for L1 production
anova_result_prod <- anova(model_L1_CV_prod_no_gain, model_L1_CV_prod_gain)
print(anova_result_prod)


# L2 Comprehension
# Define the formula for L2 comprehension without CDI_gain
formula_L2_comp_no_gain <- L2_comprehension ~ age_CDI + AWC_COUNT_Avg + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L2 comprehension without CDI_gain
model_L2_comp_no_gain <- lm(formula_L2_comp_no_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L2_comp_no_gain)

# Define the formula for L2 comprehension including CDI_gain
formula_L2_comp_gain <- L2_comprehension ~ age_CDI + AWC_COUNT_Avg + CT_COUNT_Avg + Sex + Overlap_Avg + CDI_gain
# Run the multiple linear regression for L2 comprehension with CDI_gain
model_L2_comp_gain <- lm(formula_L2_comp_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L2_comp_gain)

# Compare models using ANOVA for L2 comprehension
anova_result_L2_comp <- anova(model_L2_comp_no_gain, model_L2_comp_gain)
print(anova_result_L2_comp)

# L2 Production
# Define the formula for L2 production without CDI_gain
formula_L2_prod_no_gain <- L2_production ~ age_CDI + AWC_COUNT_Avg + CT_COUNT_Avg + Sex + Overlap_Avg
# Run the multiple linear regression for L2 production without CDI_gain
model_L2_prod_no_gain <- lm(formula_L2_prod_no_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L2_prod_no_gain)

# Define the formula for L2 production including CDI_gain
formula_L2_prod_gain <- L2_production ~ age_CDI + AWC_COUNT_Avg + CT_COUNT_Avg + Sex + Overlap_Avg + CDI_gain
# Run the multiple linear regression for L2 production with CDI_gain
model_L2_prod_gain <- lm(formula_L2_prod_gain, data = both_counts_T2)
# Print the summary of the model
summary(model_L2_prod_gain)

# Compare models using ANOVA for L2 production
anova_result_L2_prod <- anova(model_L2_prod_no_gain, model_L2_prod_gain)
print(anova_result_L2_prod)
