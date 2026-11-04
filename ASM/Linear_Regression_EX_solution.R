# ==============================================================================
# APPLIED STATISTICAL MODELLING (A.A. 2026-2027)
# Student Exercise & Solution Script:
# Multivariable Linear Regression on Academic Performance
#
# Dataset: Student_Performance_Exercise.csv
# Author:  Claudio Mazzi
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "Student_Performance_Exercise.csv" contains records for 964
# university students (950 unique complete observations after cleaning) across
# 12 variables (4 categorical and 8 quantitative):
#
# Categorical Variables:
#   1. Faculty                : University department (Engineering, Computer_Science,
#                               Economics, Biology)
#   2. Study.Mode             : Enrollment status (Full_Time, Part_Time)
#   3. Campus.Housing         : Living arrangement (On_Campus, Off_Campus, Commuter)
#   4. Scholarship            : Holds a scholarship (Yes, No)
#
# Quantitative Variables:
#   5. Weekly.Study.Hours     : Independent study hours per week (hours)
#   6. Weekly.Study.Minutes   : Independent study minutes per week (minutes) [REDUNDANT]
#   7. Attendance.Rate.pct    : Lecture attendance rate (%)
#   8. HighSchool.Score       : High school final graduation grade (60 - 100)
#   9. Social.Media.Hours.day : Daily time spent on social media / gaming (hours/day)
#  10. Sleep.Hours.day        : Average nightly sleep duration (hours/day)
#  11. Commute.Time.min       : Daily one-way commute time to campus (minutes)
#  12. Final.Grade.Index      : Overall university grade index (0 - 100) [RESPONSE]
# ==============================================================================

library(ggplot2)
library(dplyr)
try(library(summarytools), silent = TRUE)
library(corrplot)
library(car)
library(tidyverse)
library(psych)


# ==============================================================================
# STEP 1: LOAD AND INSPECT THE DATASET
# [Exercise Task 1]: Load "Student_Performance_Exercise.csv", check its dimensions,
# inspect the first 6 rows, data types, and summary statistics.
# ==============================================================================

dataset <- read.csv("Student_Performance_Exercise.csv", stringsAsFactors = FALSE)

head(dataset)
dim(dataset)      # [1] 964  12
str(dataset)
summary(dataset)


# ==============================================================================
# STEP 2: BASIC DATABASE MANAGEMENT & CONTINGENCY TABLES
# [Exercise Task 2]:
#   a) Filter Full_Time Engineering students with Final.Grade.Index >= 75.
#   b) Build a two-way contingency table between Study.Mode and Campus.Housing,
#      compute row-conditional percentages, and test independence via Chi-squared.
#   c) Build a two-way contingency table between Faculty and Scholarship and
#      verify whether they are statistically independent.
# ==============================================================================

# Create a temporary clean copy for database exercises
db_temp <- na.omit(dataset)
db_temp <- db_temp[!duplicated(db_temp), ]

# 2a) Filtering records
top_eng_students <- db_temp %>%
  filter(Faculty == "Engineering",
         Study.Mode == "Full_Time",
         Final.Grade.Index >= 75)
cat("Full-time Engineering students with Grade Index >= 75:", nrow(top_eng_students), "\n")

# 2b) Two-way contingency table 1: Study.Mode vs Campus.Housing (Dependent)
tab_mode_housing <- table(Study_Mode = db_temp$Study.Mode,
                          Housing    = db_temp$Campus.Housing)
addmargins(tab_mode_housing)
round(prop.table(tab_mode_housing, margin = 1) * 100, 1)
chisq.test(tab_mode_housing)
# Result: X-squared = 180.98, df = 2, p-value < 2.2e-16 -> Strong dependence!

# 2c) Two-way contingency table 2: Faculty vs Scholarship (Independent)
tab_fac_schol <- table(Faculty     = db_temp$Faculty,
                       Scholarship = db_temp$Scholarship)
addmargins(tab_fac_schol)
chisq.test(tab_fac_schol)
# Result: X-squared = 2.8845, df = 3, p-value = 0.4098 -> Independent (fail to reject H0)!

rm(db_temp, top_eng_students, tab_mode_housing, tab_fac_schol)


# ==============================================================================
# STEP 3: PRE-PROCESSING AND CLEANING
# [Exercise Task 3]:
#   a) Count and remove rows with missing values (NAs).
#   b) Identify and remove the redundant column ('Weekly.Study.Minutes') to
#      prevent exact multicollinearity.
#   c) Remove duplicated rows.
# ==============================================================================

# Check total missing values
sum(is.na(dataset))             # [1] 7
colSums(is.na(dataset))

# Remove rows with missing values
dataset_clean <- na.omit(dataset)

# Remove redundant column (Weekly.Study.Minutes = 60 * Weekly.Study.Hours)
dataset_clean <- dataset_clean %>% select(-c(Weekly.Study.Minutes))

# Remove duplicate rows
sum(duplicated(dataset_clean))  # [1] 8
dataset_clean <- dataset_clean[!duplicated(dataset_clean), ]

dim(dataset_clean)              # [1] 950  11
db_stud <- dataset_clean

# Select quantitative variables
qt_var <- db_stud %>% select_if(is.numeric)


# ==============================================================================
# STEP 4: EXPLORATORY DATA ANALYSIS (EDA) & STANDARDIZATION
# [Exercise Task 4]:
#   a) Plot the histogram of the response variable 'Final.Grade.Index'.
#   b) Generate the pairs.panels() scatterplot/correlation matrix.
#   c) Standardize all numeric variables and plot the correlation matrix with corrplot().
# ==============================================================================

par(mfrow = c(1, 1))
hist(db_stud$Final.Grade.Index,
     col = "orange",
     border = "black",
     main = "Distribution of Final Grade Index",
     xlab = "Final Grade Index (0-100)",
     ylab = "Frequency",
     breaks = 12)

pairs.panels(db_stud[, colnames(qt_var)],
             method = "pearson",
             hist.col = "red",
             density = TRUE,
             ellipses = TRUE)

# Standardize quantitative variables
db_stud <- as.data.frame(scale(db_stud[, colnames(qt_var)]))
head(db_stud)

corr_matrix <- cor(db_stud[, colnames(qt_var)], use = "complete.obs")
print(round(corr_matrix, 2))
corrplot(corr_matrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


# ==============================================================================
# STEP 5: SIMPLE LINEAR REGRESSION (SLR) & DIAGNOSTICS
# [Exercise Task 5]:
#   a) Fit a Simple Linear Regression of Final.Grade.Index on Weekly.Study.Hours,
#      plot the fitted line, and inspect the 4 diagnostic plots.
#   b) Fit a second Simple Linear Regression of Final.Grade.Index on
#      Attendance.Rate.pct and compare the results.
# ==============================================================================

# 5a) SLR 1: Final.Grade.Index ~ Weekly.Study.Hours
model_SLR <- lm(Final.Grade.Index ~ Weekly.Study.Hours, data = db_stud)
summary(model_SLR)
# Slope = 0.8353 (p < 2e-16 ***), R-squared = 0.6978

ggplot(db_stud, aes(x = Weekly.Study.Hours, y = Final.Grade.Index)) +
  geom_point(size = 1.6, alpha = 0.85) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Simple Linear Regression: Final Grade Index vs. Weekly Study Hours",
       x = "Weekly Study Hours (standardized)",
       y = "Final Grade Index (standardized)")

par(mfrow = c(2, 2))
plot(model_SLR, sub = "")

# 5b) SLR 2: Final.Grade.Index ~ Attendance.Rate.pct
model_SLR_new <- lm(Final.Grade.Index ~ Attendance.Rate.pct, data = db_stud)
summary(model_SLR_new)
# Slope = 0.5254 (p < 2e-16 ***), R-squared = 0.2761

ggplot(db_stud, aes(x = Attendance.Rate.pct, y = Final.Grade.Index)) +
  geom_point(size = 1.6, alpha = 0.85) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simple Linear Regression: Final Grade Index vs. Attendance Rate",
       x = "Attendance Rate (%, standardized)",
       y = "Final Grade Index (standardized)")

par(mfrow = c(2, 2))
plot(model_SLR_new, sub = "")


# ==============================================================================
# STEP 6: MULTIPLE LINEAR REGRESSION (MLR) & MULTICOLLINEARITY
# [Exercise Task 6]:
#   a) Fit the Multiple Linear Regression model including all 6 quantitative
#      predictors and interpret the estimated coefficients and p-values.
#   b) Check Variance Inflation Factors (VIF) and diagnostic plots.
# ==============================================================================

model_MLR <- lm(Final.Grade.Index ~ Weekly.Study.Hours + Attendance.Rate.pct +
                  HighSchool.Score + Social.Media.Hours.day +
                  Sleep.Hours.day + Commute.Time.min,
                data = db_stud)

summary(model_MLR)
# Summary of findings:
# - Weekly.Study.Hours     ( 0.6456, p < 2e-16 ***): Strongest positive effect.
# - Attendance.Rate.pct    ( 0.3139, p < 2e-16 ***): Strong positive effect.
# - HighSchool.Score       ( 0.1205, p < 2e-16 ***): Significant positive baseline effect.
# - Social.Media.Hours.day (-0.1598, p < 2e-16 ***): Significant negative effect.
# - Sleep.Hours.day        ( 0.0249, p = 0.0171 * ): Marginally significant positive effect.
# - Commute.Time.min       (-0.0053, p = 0.6600   ): NOT statistically significant!
#   (Its negative bivariate correlation with grades, r = -0.39, was confounded by
#   Attendance.Rate.pct, r = -0.57: students with long commutes attend fewer
#   lectures, and once Attendance.Rate.pct is controlled for, commute duration
#   has no direct effect on exam performance).
# - Multiple R-squared = 0.9088 (Adjusted R-squared = 0.9082).

vif(model_MLR)
# All VIFs are between 1.12 and 1.91 (< 5, no multicollinearity issues).

par(mfrow = c(2, 2))
plot(model_MLR, sub = "")


# ==============================================================================
# STEP 7: INTERACTION EFFECTS IN MULTIPLE LINEAR REGRESSION
# [Exercise Task 7]:
#   Test whether the effectiveness of Weekly.Study.Hours depends on
#   Attendance.Rate.pct by including their interaction term.
# ==============================================================================

model_MLR_int <- lm(Final.Grade.Index ~ Weekly.Study.Hours * Attendance.Rate.pct +
                      HighSchool.Score + Social.Media.Hours.day +
                      Sleep.Hours.day + Commute.Time.min,
                    data = db_stud)

summary(model_MLR_int)
# The interaction term 'Weekly.Study.Hours:Attendance.Rate.pct' is positive
# (0.1702) and highly significant (t = 21.95, p < 2e-16 ***), showing that
# self-study hours are substantially more productive when combined with high
# lecture attendance. R-squared rises from 0.9088 to 0.9396!

anova(model_MLR, model_MLR_int)
par(mfrow = c(2, 2))
plot(model_MLR_int, sub = "")
par(mfrow = c(1, 1))

