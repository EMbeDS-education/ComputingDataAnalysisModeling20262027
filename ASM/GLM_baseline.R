# Step 1: Install and Load Required Packages
install.packages("nnet")
install.packages("dplyr")
install.packages(c("tidyverse", "GGally", "MASS", "pscl"))

library(dplyr)
library(tidyverse)
library(GGally)
library(ggplot2)
library(pROC)
library(stats)
library(nnet)
library(MASS)    # For ordinal logistic regression
library(pscl)    # For calculating Pseudo R²
library(ggcorrplot)


# Step 1: Logit and Probit Model on heartdisease db
# Load the dataset

# Preprocess the data
# One-hot encode categorical variables for 'ChestPainType', 'RestingECG', and 'ST_Slope'
# Drop original factor columns


# Logistic regression model (logit)
# Probit regression model
# Summaries of both models


# Calculating pseudo-R2 values for both models
# Pseudo-R2 for logit model
# Pseudo-R2 for probit model

# Predictions and confusion matrix
# ROC curve and AUC


################################################################################

# Step 2: Ordinal Logit on wine_quality database
# Load the dataset
# Explore the dataset


# Distribution of the quality variable
ggplot(df_wne, aes(x = quality)) + geom_bar(fill = "blue") + theme_minimal() +
  labs(title = "Distribution of Wine Quality")

# Correlation matrix (optional, for exploration purposes)
df_wne_numeric <- dplyr::select_if(df_wne, is.numeric)
df_corr <- cor(df_wne_numeric, method = "kendall")
ggcorrplot(df_corr, type = "lower", lab = TRUE, lab_size = 3)

# Generate pairplot colored by 'quality'
df_no_quality <- dplyr::select(df_wne, -quality)
quality_levels <- sort(unique(df_wne$quality))
quality_colors <- scales::hue_pal()(length(quality_levels))

custom_cor <- function(data, mapping, ...){
  ggally_cor(data, mapping, ..., size = 3, digits = 2, method = "kendall") +
    theme(text = element_text(size = 8))
}

ggpairs(df_no_quality, 
        aes(color = as.factor(df_wne$quality), alpha = 0.6),
        upper = list(continuous = custom_cor), 
        lower = list(continuous = wrap("points", alpha = 0.6)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) + 
  scale_color_manual(values = quality_colors, name = "Quality") + 
  scale_fill_manual(values = quality_colors, name = "Quality") +
  theme_minimal() +
  labs(title = "Pairplot of Wine Quality Data with Correlations") +
  theme(legend.position = "right")

# Ordinal logistic regression
# Display model summary
summary(ordinal_model)

# 1. Log-Likelihood and Likelihood Ratio Test
# 2. BIC

# 4. Compute Accuracy, deviance and the confusion matrix
# Predicts the categories with the highest probability


################################################################################

# Step 3: Multinomial Logit multinomial_logit_nation.csv
# Load dataset

# Put continent variable as factor

# Multinomial logit execution
model_multi <- multinom(continent ~ life_expectancy + gdp_per_capita + 
                          health_expenditure + education_index, data = df_multi)
# Summary of results
# Coefficients and standard errors
coefficients <- summary(model_multi)$coefficients
std_errors <- summary(model_multi)$standard.errors

# Z-values
# p-value
# Print p-value

# Analysis of coefficients - computing odds ratio
# Predicted probabilities
# Adding probablities to database

# Observed frequencies
# Predicted frequencies


# Combine observed and predicted data
# Plottin results
combined_long <- tidyr::pivot_longer(combined_df, 
                                     cols = c("observed_count", "predicted_count"),
                                     names_to = "Type",
                                     values_to = "Count")
combined_long$Type <- recode(combined_long$Type, "observed_count" = "Observed", "predicted_count" = "Predicted")

# Histogram with side-by-sed bars
ggplot(combined_long, aes(x = continent, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(x = "Continent", y = "Count", title = "") +
  scale_fill_manual(name = "Legend", values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()


################################################################################