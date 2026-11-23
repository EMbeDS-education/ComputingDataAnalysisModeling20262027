# ==============================================================================
# APPLIED STATISTICAL MODELLING (A.A. 2026-2027)
# Student Laboratory Assignment:
# Generalized Linear Models (GLMs) & Data Management in R
#
# Dataset: Customer_Churn_GLM_Exercise.csv
# Author:  Claudio Mazzi
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "Customer_Churn_GLM_Exercise.csv" contains subscription records
# for a telecommunications/SaaS service (N = 1,006 raw observations across 12 variables):
#
# Binary Response Variable:
#   1. Churn                : Binary churn status (1 = Customer Churned / Cancelled,
#                             0 = Customer Retained)
#
# Ordinal Outcome Variable (for Step 7):
#   2. Satisfaction_Tier    : Post-service survey tier (Low, Medium, High)
#
# Categorical Predictors:
#   3. Contract_Type        : Subscription contract (Month-to-Month, One_Year, Two_Year)
#   4. Payment_Method       : Payment mechanism (Electronic_Check, Bank_Transfer, Credit_Card)
#   5. Paperless_Billing    : Invoicing preference (Yes, No)
#
# Quantitative Predictors:
#   6. Age                  : Customer age in years (18 - 75)
#   7. Tenure_Months        : Subscription tenure in months (1 - 72)
#   8. Tenure_Days          : Redundant tenure in days (= Tenure_Months * 30.4)
#   9. Monthly_Usage_Hours  : Average monthly platform usage in hours (10 - 250)
#  10. Support_Tickets      : Number of customer service inquiries filed (0 - 8)
#  11. Monthly_Fee_EUR      : Monthly recurring subscription fee in Euros (19.99 - 119.99)
#  12. CustomerID           : Unique customer identifier
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD REQUIRED LIBRARIES
# ------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(pROC)
library(pscl)
library(MASS)
library(brant)


# ==============================================================================
# STEP 1: LOAD AND INSPECT THE DATASET
# ==============================================================================
# [Exercise Task 1]:
#   a) Load the CSV dataset "Customer_Churn_GLM_Exercise.csv" into an object
#      named 'dataset'.
#   b) Check the dimensions of the dataset (number of rows and columns).
#   c) Inspect the first 6 rows and the last 6 rows of the dataset.
#   d) Check the internal data structure and data types of all columns.
#   e) Display descriptive summary statistics for each variable.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 2: DATA PRE-PROCESSING AND CLEANING
# ==============================================================================
# [Exercise Task 2]:
#   a) Duplicate rows:
#      - Check for duplicate rows in the dataset using duplicated().
#      - Count how many duplicates exist and remove them.
#   b) Missing values (NAs):
#      - Count the total number of missing values across the dataset.
#      - Identify which columns contain missing values using colSums(is.na(...)).
#      - Remove all rows with missing values (save as 'dataset_clean').
#   c) Redundant columns:
#      - Verify that 'Tenure_Days' is collinear with 'Tenure_Months' (cor()).
#      - Remove 'Tenure_Days' and 'CustomerID' from 'dataset_clean'.
#   d) Verify the final dimensions of 'dataset_clean' (should be exactly 1,000 rows).
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 3: EXPLORATORY DATA ANALYSIS & CONTINGENCY TABLES
# ==============================================================================
# [Exercise Task 3]:
#   a) Examine the baseline churn distribution:
#      - Compute the frequency and percentage of churned vs. retained customers.
#   b) Contingency Table 1 (Contract Type vs. Churn):
#      - Build a two-way frequency table between Contract_Type and Churn.
#      - Compute row percentages using prop.table(..., margin = 1).
#      - Perform Pearson's Chi-squared test of independence (chisq.test()).
#      - Interpret the test: Does customer churn depend on contract type?
#   c) Contingency Table 2 (Payment Method vs. Churn):
#      - Build a two-way table between Payment_Method and Churn.
#      - Perform Chi-squared test and interpret the result.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 4: BINARY LOGISTIC REGRESSION (LOGIT MODEL)
# ==============================================================================
# [Exercise Task 4]:
#   a) Fit a binary logistic regression model predicting 'Churn' from:
#      Age, Tenure_Months, Monthly_Usage_Hours, Support_Tickets, Monthly_Fee_EUR,
#      Contract_Type, Payment_Method, and Paperless_Billing.
#      Use glm(..., family = binomial(link = "logit")).
#   b) Display the summary output of the model.
#   c) Identify which predictors are statistically significant at alpha = 0.05.
#   d) Compute the Odds Ratios (exp(beta)) and their 95% confidence intervals.
#   e) Interpret the odds ratio for 'Support_Tickets' and 'Tenure_Months'.
#   f) Compute McFadden's pseudo-R2 using pscl::pR2().
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 5: PROBIT REGRESSION & MODEL COMPARISON
# ==============================================================================
# [Exercise Task 5]:
#   a) Fit a probit regression model with the identical predictors:
#      Use glm(..., family = binomial(link = "probit")).
#   b) Display the summary of the probit model.
#   c) Compare the Logit and Probit models in terms of:
#      - AIC (Akaike Information Criterion)
#      - BIC (Bayesian Information Criterion)
#      - McFadden's pseudo-R2
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 6: CLASSIFICATION DIAGNOSTICS & ROC CURVE
# ==============================================================================
# [Exercise Task 6]:
#   a) Generate predicted probabilities for the Logit model using predict(..., type = "response").
#   b) Convert probabilities into binary predictions using a 0.5 decision threshold.
#   c) Build the Confusion Matrix (Predicted vs. Actual Churn).
#   d) Calculate:
#      - Accuracy (Overall proportion of correct classifications)
#      - Sensitivity / Recall (True Positive Rate: predicting Churned when Churned)
#      - Specificity (True Negative Rate: predicting Retained when Retained)
#   e) Plot the ROC curve and compute the Area Under the Curve (AUC) using pROC.
# ------------------------------------------------------------------------------

# Write your code here:




# ==============================================================================
# STEP 7: ORDINAL LOGISTIC REGRESSION (CUSTOMER SATISFACTION)
# ==============================================================================
# [Exercise Task 7]:
#   a) Define 'Satisfaction_Tier' as an ordered factor with levels:
#      'Low' < 'Medium' < 'High'.
#   b) Fit a proportional odds ordinal logit model predicting 'Satisfaction_Tier'
#      from: Monthly_Usage_Hours, Support_Tickets, Monthly_Fee_EUR, and Contract_Type.
#      Use MASS::polr(..., Hess = TRUE).
#   c) Display the summary output of the model.
#   d) Interpret the sign and significance of 'Monthly_Usage_Hours' and 'Support_Tickets'.
#   e) Perform a Likelihood Ratio Test comparing the full model against a null model.
#   f) Compute the ordinal confusion matrix and overall classification accuracy.
# ------------------------------------------------------------------------------

# Write your code here:


