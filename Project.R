#install and load required libraries
install.packages("mice")
library(tidyverse)
library(mice)

#Load and read the CSV file
file_path <- "StressLevelDataset.csv"

# Read the CSV file into a data frame
dataset <- read.csv(file_path)

#Display Datset Head
head(dataset)

# Fit a linear regression model to predict stress_level
# based on various predictor variables using the lm function
academic_model <- lm(stress_level ~ academic_performance + study_load + teacher_student_relationship + future_career_concerns,data = dataset)

# Generate a summary of the linear regression model
# to provide insights into its statistical performance and coefficients
summary(academic_model)

# Perform an analysis of variance (ANOVA) to assess the relationship
# between self-esteem and social support in the dataset
anova_result <- aov(self_esteem ~ social_support, data = dataset)

# Generate a summary of the analysis of variance (ANOVA) results
# to provide insights into the statistical significance of the model
summary(anova_result)


# Convert the 'social_support' variable to a factor
dataset$social_support <- as.factor(dataset$social_support)

# Perform an analysis of variance (ANOVA) to assess the relationship
# between 'self_esteem' and the categorical variable 'social_support'
anova_result <- aov(self_esteem ~ social_support, data = dataset)

# Perform Tukey's Honestly Significant Difference (HSD) test
# to compare means between different levels of 'social_support'
tukey_result <- TukeyHSD(anova_result)

# Print the results of the Tukey HSD test
print(tukey_result)

# Set a seed for reproducibility
set.seed(123)

# Create a new dataset 'dataset_mcar' based on 'dataset'
# where missing values are introduced in each column (MCAR mechanism)
# with a missingness rate of 20%
dataset_mcar <- dataset %>%
  mutate_all(~ifelse(runif(n()) < 0.2, NA, .))

# Display the first few rows of the modified dataset
head(dataset_mcar)

# Impute missing values in 'dataset_mcar' using mean imputation
dataset_mcar_imputed <- dataset_mcar %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Display the first few rows of the imputed dataset
head(dataset_mcar_imputed)

# Fit a linear regression model to predict 'stress_level'
# based on various predictor variables using the imputed dataset
academic_model_imputed <- lm(
  stress_level ~ academic_performance + study_load +
    teacher_student_relationship + future_career_concerns,
  data = dataset_mcar_imputed
)

# Print a summary of the imputed model
summary(academic_model_imputed)


# Convert 'social_support' to a factor variable (if not already)
dataset_mcar_imputed$social_support <- as.factor(dataset_mcar_imputed$social_support)

# Fit an Analysis of Variance (ANOVA) model
anova_result_imputed <- aov(self_esteem ~ social_support, data = dataset_mcar_imputed)

# Print the summary of the ANOVA results
summary(anova_result_imputed)

# Perform Tukey's Honestly Significant Difference (HSD) test
tukey_result_imputed <- TukeyHSD(anova_result_imputed)

# Print the results of Tukey's HSD test
print(tukey_result_imputed)


# Create a new dataset 'dataset_mnar_noise' based on 'dataset'
# where missing values are introduced in each column (MNAR mechanism)
# with a missingness rate of 10% multiplied by normally distributed noise
dataset_mnar_noise <- dataset %>%
  mutate_all(~ifelse(runif(n()) < 0.1 * rnorm(n()), NA, .))


# Impute missing values in 'dataset_mnar_noise' using Multiple Imputation by Chained Equations (MICE)
# with 5 imputations and a normal regression imputation method
dataset_mnar_imputed <- mice(dataset_mnar_noise, m = 5, method = "norm")

# Extract the completed dataset from the imputed results
completed_data <- complete(dataset_mnar_imputed, action = "long")

# Fit a multiple linear regression model to predict 'self_esteem'
# based on the imputed and completed dataset
mnar_model_imputed <- lm(self_esteem ~ social_support, data = completed_data)

# Print a summary of the imputed multiple linear regression model
summary(mnar_model_imputed)

# Fit an Analysis of Variance (ANOVA) model on the imputed and completed dataset
anova_result_mnar_imputed <- aov(self_esteem ~ social_support, data = completed_data)

# Print the summary of the ANOVA results
summary(anova_result_mnar_imputed)

#Convert 'social_support' to a factor in the imputed dataset
completed_data$social_support <- as.factor(completed_data$social_support)

# Refit ANOVA model after converting 'social_support' to a factor
anova_result_mnar_imputed <- aov(self_esteem ~ social_support, data = completed_data)

# Perform Tukey's Honestly Significant Difference (HSD) test
tukey_result_mnar_imputed <- TukeyHSD(anova_result_mnar_imputed)

# Print the results of Tukey's HSD test
print(tukey_result_mnar_imputed)
