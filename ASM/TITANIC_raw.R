###############################################################
# TITANIC DATA ANALYSIS — STUDENT TASK SCRIPT (NO CODE)
# Follow each instruction and write the R code yourself.
# RESEARCH QUESTIONS
# -------------------------------------------------------------
# 1) s it true that more British people died than Americans because they queued?
#
# 2) Are there differences in the amount paid for tickets
#    by nationality?
#
# 3) Is there a relationship between class and survival,
#    controlling for the other independent variables?
#
# Your task is to explore the dataset and answer these
# questions using descriptive statistics, graphical tools,
# and statistical modelling (GLM and multivariate methods).
###############################################################


# =============================================================
# TASK 1 — Load and Inspect the Dataset
# =============================================================
# • Load the Titanic CSV dataset into R.
# • Print the first six rows.
# • Inspect the structure of the dataset (variables, types).
# • Compute summary statistics for all variables.
# • Identify which variables contain missing values.


# =============================================================
# TASK 2 — Handle Missing Data
# =============================================================
# • Compute the number of missing values per variable.
# • Replace missing Age values with the median age of passengers.
# • Identify the most frequent category in Embarked.
# • Replace missing Embarked values with that category.
# • Create a new binary variable “CabinKnown”:
#      CabinKnown = 1 if Cabin is not missing, else 0.


# =============================================================
# TASK 3 — Recode Variables
# =============================================================
# • Convert Survived to a factor (“Died”, “Survived”).
# • Convert Pclass to an ordered factor.
# • Convert Sex and Embarked to categorical (factor) variables.


# =============================================================
# TASK 4 — Univariate Exploratory Data Analysis
# =============================================================
# Create the following plots:
# • A bar plot showing counts of survivors vs. non-survivors.
# • A histogram of Age (choose a number of bins, e.g. 30).
# • A histogram of Fare (note its skewness).
# • Write a short comment for each plot.


# =============================================================
# TASK 5 — Bivariate EDA with Respect to Survival
# =============================================================
# Produce and comment:
# • A bar plot showing survival proportion by Pclass.
# • A bar plot showing survival proportion by Sex.
# • A histogram of Age, filled by Survived.
# • Based on the plots, hypothesize which variables
#   might be strong predictors of survival.


# =============================================================
# TASK 6 — Fit a Logistic Regression (GLM)
# =============================================================
# • Fit a logistic regression model with Survived as outcome
#   and the following predictors:
#     Pclass, Sex, Age, SibSp, Parch, Fare,
#     Embarked, CabinKnown
# • Print the model summary.
# • Identify significant predictors.
# • Interpret at least three coefficients in your own words.


# =============================================================
# TASK 7 — Evaluate the Model
# =============================================================
# • Compute McFadden’s pseudo-R².
# • Compute predicted probabilities.
# • Classify predictions using a 0.5 threshold.
# • Create a confusion matrix.
# • Compute accuracy, sensitivity, specificity.
# • Plot the ROC curve.
# • Compute the AUC value.
# • Write a brief conclusion on model performance.
