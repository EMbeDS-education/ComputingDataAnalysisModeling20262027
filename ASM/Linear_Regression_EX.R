# ==============================================================================
# APPLIED STATISTICAL MODELLING (A.A. 2026-2027)
# Student Laboratory Assignment:
# Multivariable Linear Regression & Database Management in R
#
# Dataset: Linear_Regression_Exercise.csv
# Author:  Claudio Mazzi
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "Linear_Regression_Exercise.csv" contains records for 964
# university students across 12 variables (4 categorical and 8 quantitative):
#
# Categorical Variables:
#   1. Faculty                : University department (Engineering, Computer_Science,
#                               Economics, Biology)
#   2. Study.Mode             : Enrollment status (Full_Time, Part_Time)
#   3. Campus.Housing         : Living arrangement (On_Campus, Off_Campus, Commuter)
#   4. Scholarship            : Holds a merit/income scholarship (Yes, No)
#
# Quantitative Variables:
#   5. Weekly.Study.Hours     : Independent study hours per week (hours)
#   6. Weekly.Study.Minutes   : Independent study minutes per week (minutes)
#   7. Attendance.Rate.pct    : Lecture attendance rate (%)
#   8. HighSchool.Score       : High school final graduation grade (scale: 60 - 100)
#   9. Social.Media.Hours.day : Daily time spent on social media / gaming (hours/day)
#  10. Sleep.Hours.day        : Average nightly sleep duration (hours/day)
#  11. Commute.Time.min       : Daily one-way commute time to campus (minutes)
#  12. Final.Grade.Index      : Overall university grade index (scale: 0 - 100)
#                               --> RESPONSE VARIABLE
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
try(library(summarytools), silent = TRUE)
library(corrplot)
library(car)
library(tidyverse)
library(psych)


# ==============================================================================
# STEP 1: LOAD AND INSPECT THE DATASET
# ==============================================================================
# [Exercise Task 1]:
#   a) Load the CSV dataset "Linear_Regression_Exercise.csv" into an object
#      named 'dataset'.
#   b) Check the dimensions of the dataset (number of rows and columns).
#   c) Inspect the first 6 rows and the last 6 rows of the dataset.
#   d) Check the internal data structure and data types of all columns.
#   e) Display descriptive summary statistics for each variable.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 2: BASIC DATABASE MANAGEMENT & CONTINGENCY TABLES
# ==============================================================================
# [Exercise Task 2]:
#   a) Filtering records:
#      Select all Full-Time students enrolled in the Engineering faculty who have
#      a Final.Grade.Index >= 75. Count how many students meet these criteria.
#
#   b) Contingency Table 1 (Testing Categorical Dependence):
#      - Build a two-way frequency table between Study.Mode and Campus.Housing.
#      - Add row and column marginal totals.
#      - Compute row-conditional proportions (percentages summing to 100% per row).
#      - Perform Pearson's Chi-squared test of independence (chisq.test()).
#      - Comment on the p-value: is housing status independent of study mode?
#
#   c) Contingency Table 2 (Testing Categorical Independence):
#      - Build a two-way frequency table between Faculty and Scholarship.
#      - Add marginal totals and perform Pearson's Chi-squared test of independence.
#      - Interpret the test result: is scholarship allocation dependent on faculty?
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 3: DATA PRE-PROCESSING AND CLEANING
# ==============================================================================
# [Exercise Task 3]:
#   a) Missing values (NAs):
#      - Count the total number of missing values in the dataset.
#      - Check how many NAs are present in each column.
#      - Remove all rows containing missing values (save as 'dataset_clean').
#
#   b) Multicollinearity & Redundant columns:
#      - Identify which column provides redundant information that would lead
#        to exact multicollinearity if kept.
#      - Remove this redundant column from 'dataset_clean'.
#
#   c) Duplicate rows:
#      - Check how many duplicate rows are present in the dataset.
#      - Remove the duplicate rows.
#      - Check the final dimensions of the cleaned dataset (it should be 950 x 11).
#      - Create a subset of numeric variables named 'qt_var'.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 4: EXPLORATORY DATA ANALYSIS (EDA) & STANDARDIZATION
# ==============================================================================
# [Exercise Task 4]:
#   a) Distribution of the Response Variable:
#      - Plot a histogram of 'Final.Grade.Index' to verify whether the distribution
#        is symmetric and approximately normal.
#
#   b) Pairwise Visualizations and Correlations:
#      - Use pairs.panels() on the quantitative variables to inspect pairwise
#        scatterplots, density curves, and Pearson correlation coefficients.
#
#   c) Standardization:
#      - Standardize all quantitative features using scale() so that coefficients
#        can be directly compared across different units of measurement.
#
#   d) Correlation Matrix:
#      - Compute the Pearson correlation matrix for the quantitative variables.
#      - Visualize the correlation matrix using corrplot().
#      - Identify which predictors show the strongest positive and negative
#        correlations with Final.Grade.Index.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 5: SIMPLE LINEAR REGRESSION (SLR) & DIAGNOSTIC CHECKS
# ==============================================================================
# [Exercise Task 5]:
#   a) Simple Linear Regression 1:
#      - Fit a Simple Linear Regression model (named 'model_SLR') predicting
#        Final.Grade.Index using the single strongest predictor (Weekly.Study.Hours).
#      - Display the model summary (summary(model_SLR)).
#      - Interpret the estimated slope coefficient, t-statistic, p-value, and R-squared.
#      - Plot the scatterplot with the fitted regression line using ggplot2.
#      - Generate and inspect the 4 diagnostic plots (par(mfrow = c(2, 2)); plot(model_SLR)).
#        Comment on linearity, normality of residuals, homoscedasticity, and leverage.
#
#   b) Simple Linear Regression 2:
#      - Fit a second Simple Linear Regression model (named 'model_SLR_new')
#        predicting Final.Grade.Index from Attendance.Rate.pct.
#      - Display the summary, plot the regression line, and compare the R-squared
#        and diagnostics with the previous model.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 6: MULTIPLE LINEAR REGRESSION (MLR) & MULTICOLLINEARITY
# ==============================================================================
# [Exercise Task 6]:
#   a) Fit a Multiple Linear Regression model (named 'model_MLR') predicting
#      Final.Grade.Index using all 6 quantitative predictors simultaneously:
#      Weekly.Study.Hours, Attendance.Rate.pct, HighSchool.Score,
#      Social.Media.Hours.day, Sleep.Hours.day, and Commute.Time.min.
#
#   b) Model Evaluation:
#      - Display the summary of the model (summary(model_MLR)).
#      - Interpret the coefficients, significance levels (p-values), and the overall
#        Multiple and Adjusted R-squared.
#      - Look closely at Commute.Time.min: why is it not statistically significant
#        in the multiple regression even though it showed a negative correlation
#        with grades in the bivariate EDA? (Hint: confounding effect with attendance).
#
#   c) Multicollinearity Diagnostics:
#      - Compute the Variance Inflation Factor for each predictor using vif(model_MLR).
#      - Verify that all VIF values remain well below the rule-of-thumb threshold of 5.
#
#   d) Residual Diagnostics:
#      - Generate and evaluate the 4 diagnostic plots for model_MLR.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 7: TESTING FOR INTERACTION EFFECTS IN MULTIPLE LINEAR REGRESSION
# ==============================================================================
# [Exercise Task 7]:
#   a) Interaction Formulation:
#      - Test the hypothesis that the effectiveness of independent study hours
#        (Weekly.Study.Hours) depends positively on attending lectures
#        (Attendance.Rate.pct).
#      - Fit a new model (named 'model_MLR_int') that includes the interaction term:
#        Weekly.Study.Hours * Attendance.Rate.pct alongside all other predictors.
#
#   b) Model Assessment & Comparison:
#      - Display summary(model_MLR_int).
#      - Is the interaction coefficient statistically significant? What does its sign imply?
#      - Compare the Multiple R-squared and Residual Standard Error between model_MLR
#        and model_MLR_int.
#      - Perform a formal model comparison using an ANOVA F-test (anova(model_MLR, model_MLR_int)).
#      - Generate the 4 diagnostic plots for the interaction model.
# ------------------------------------------------------------------------------

# Write your code here:



# ==============================================================================
# END OF ASSIGNMENT SCRIPT
# ==============================================================================

