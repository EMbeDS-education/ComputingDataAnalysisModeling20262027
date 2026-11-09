# ==============================================================================
# APPLIED STATISTICAL MODELLING (A.A. 2026-2027)
# Lab 1: Introduction to R, Database Management, and Linear Regression
#
# Author: Claudio Mazzi
#         Department of Computer Science, University of Pisa
#         MeS Laboratory, Sant'Anna School for Advanced Studies, Pisa
#         Email: claudio.mazzi@santannapisa.it
#
# Description:
# This script provides a complete, step-by-step tutorial for students approaching
# R and statistical data analysis for the first time. It is divided into two
# main pedagogical parts:
#   PART I  (Sections 1 - 3): R fundamentals and basic database management
#           (loading data, inspecting structures, filtering, modifying records,
#           creating/deleting variables and rows, grouped summaries, and
#           two-way contingency tables with Chi-squared independence tests).
#   PART II (Sections 4 - 8): Complete Linear Regression workflow matching the
#           course Toolkit (Data Pre-processing, Exploratory Data Analysis,
#           Simple Linear Regression, Diagnostic Checks, Multiple Linear
#           Regression, Multicollinearity via VIF, and Interaction Effects).
# ==============================================================================


# ==============================================================================
# SECTION 1: GETTING STARTED WITH R & PACKAGE MANAGEMENT
# ==============================================================================

# In R, the hash symbol (#) is used to write comments. Anything following '#'
# on the same line is ignored by the R interpreter.

# 1.1 Using the Built-in Help System
# Whenever you want to learn how a function works, which arguments it accepts,
# or see practical examples, use help() or the question mark operator '?'.
help(cbind)      # Opens documentation for cbind() (column-bind)
?read.csv        # Equivalent shortcut to open documentation for read.csv()
example(mean)    # Runs the official examples from the documentation of mean()

# 1.2 Working Directory and Workspace Inspection
# The working directory is the default folder where R looks for files to read
# and saves output files.
getwd()          # Prints the current working directory path
setwd("/Users/claudiomazzi/Documents/PhD/ASM/2026_2027/1_Linear_Regression_Toolkit")  # Uncomment and edit to change working directory

# 1.3 Installing and Loading Required Packages
# Packages extend base R with specialized functions for data manipulation,
# visualization, and statistical modeling.
# NOTE: You only need to install a package ONCE on your computer using
# install.packages(), but you must load it EVERY time you start a new R session
# using library().

required_packages <- c(
  "ggplot2",       # Grammar of graphics for high-quality plots
  "dplyr",         # Verbs for tabular data manipulation (filter, select, mutate)
  "summarytools",  # Comprehensive descriptive summary tables
  "corrplot",      # Visual representation of correlation matrices
  "car",           # Companion to Applied Regression (diagnostics, VIF)
  "tidyverse",     # Ecosystem of packages for data science
  "psych"          # Psychological & statistical tools (pairs.panels)
)

# Install any missing packages automatically:
missing_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

# Load the libraries into the current R session
library(ggplot2)
library(dplyr)
# Note: On macOS, 'summarytools' requires XQuartz (tcltk); try() prevents errors if XQuartz is absent
try(library(summarytools), silent = TRUE)
library(corrplot)
library(car)
library(tidyverse)
library(psych)


# ==============================================================================
# SECTION 2: LOADING AND EXPLORING THE DATABASE
# ==============================================================================

# We load the residential real estate dataset "Real_Estate_Valuation.csv"
# using read.csv(). Ensure the file is in your working directory or provide
# the full path.
dataset <- read.csv("Real_Estate_Valuation.csv", stringsAsFactors = FALSE)

# 2.1 Inspecting the Dataset Structure and Dimensions
# View the first 6 rows (default) and last 6 rows of the table
head(dataset)
tail(dataset)

# To open the spreadsheet-like viewer in RStudio, you can run:
View(dataset)

# Check the dimensions of the database: [1] number of rows (observations),
# [2] number of columns (variables)
dim(dataset)     # Returns: 1210 rows and 12 columns
nrow(dataset)    # Number of rows only
ncol(dataset)    # Number of columns only

# List the variable (column) names
colnames(dataset)

# Inspect the internal data types of each column:
# - 'chr' (character): text / categorical strings (e.g., City.Zone, Building.Type)
# - 'num' / 'int' (numeric / integer): quantitative measurements
str(dataset)

# Compute summary statistics for every column:
# For numeric columns: Min, 1st Quartile, Median, Mean, 3rd Quartile, Max, and NAs.
# For character columns: length and class.
summary(dataset)


# ==============================================================================
# SECTION 3: FUNDAMENTAL DATABASE MANAGEMENT IN R (FOR BEGINNERS)
# ==============================================================================
# Before jumping into regression modeling, it is essential to know how to
# handle, query, modify, and cross-tabulate a database in R.
# We create a working copy called 'db_demo' so we can practice freely without
# altering our original raw 'dataset'.

db_demo <- dataset

# Remove missing values and duplicates on our demo copy first for clean examples
db_demo <- na.omit(db_demo)
db_demo <- db_demo[!duplicated(db_demo), ]

# ------------------------------------------------------------------------------
# 3.1 Accessing and Subsetting Data (Indexing Rows and Columns)
# ------------------------------------------------------------------------------
# A data.frame in R is indexed as: data_frame[row_indices, column_indices]

# Access a single column as a vector using the '$' operator:
head(db_demo$Market.Price.kEUR)

# Access specific rows and columns by position:
db_demo[1:5, 1:4]                              # First 5 rows, first 4 columns
db_demo[c(1, 10, 20), c("City.Zone", "Building.Type", "Market.Price.kEUR")]

# Filter rows based on logical conditions:
# Example A (Base R): Select Villas located in the "North" zone with price > 450 kEUR
villas_north <- subset(db_demo,
                       City.Zone == "North" & Building.Type == "Villa" & Market.Price.kEUR > 450)
head(villas_north)
cat("Number of Villas in North zone above 450 kEUR:", nrow(villas_north), "\n")

# Example B (dplyr syntax using the pipe operator '%>%'):
# Filter apartments in the Center with Neighborhood.Score >= 70
central_apartments <- db_demo %>%
  filter(City.Zone == "Center",
         Building.Type == "Apartment",
         Neighborhood.Score >= 70)
head(central_apartments)

# ------------------------------------------------------------------------------
# 3.2 Sorting / Ordering Records
# ------------------------------------------------------------------------------
# Sort properties by Market.Price.kEUR in descending order (most expensive first)
# Base R approach:
db_sorted_base <- db_demo[order(-db_demo$Market.Price.kEUR), ]
head(db_sorted_base[, c("City.Zone", "Building.Type", "Living.Area.sqm", "Market.Price.kEUR")], 5)

# dplyr approach:
db_sorted_dplyr <- db_demo %>%
  arrange(desc(Market.Price.kEUR))
head(db_sorted_dplyr[, c("City.Zone", "Building.Type", "Living.Area.sqm", "Market.Price.kEUR")], 5)

# ------------------------------------------------------------------------------
# 3.3 Modifying Existing Data and Creating New Variables
# ------------------------------------------------------------------------------

# A) Creating a new numerical variable from existing columns:
# Compute the price per square meter (in EUR/sqm):
# Since Market.Price.kEUR is in thousands of Euros, we multiply by 1000.
db_demo$Price.per.sqm <- round((db_demo$Market.Price.kEUR * 1000) / db_demo$Living.Area.sqm, 2)
summary(db_demo$Price.per.sqm)

# B) Creating a categorical variable from a numerical one (Discretization):
# Using ifelse() for a binary indicator (High vs Standard Energy Efficiency):
db_demo$Green.Home <- ifelse(db_demo$Energy.Class %in% c("A", "B"), "High_Efficiency", "Standard")
table(db_demo$Green.Home)

# Using cut() to divide Market.Price.kEUR into 3 market tiers:
#   - Budget:    <= 350 kEUR
#   - Mid-Range: (350, 500] kEUR
#   - Luxury:    > 500 kEUR
db_demo$Price.Tier <- cut(
  db_demo$Market.Price.kEUR,
  breaks = c(-Inf, 350, 500, Inf),
  labels = c("Budget", "Mid-Range", "Luxury")
)
table(db_demo$Price.Tier)

# C) Modifying specific values conditionally (Recoding / Updating records):
# Suppose we want to rename "Centralized" heating to "Central_Heating"
db_demo$Heating.System[db_demo$Heating.System == "Centralized"] <- "Central_Heating"
table(db_demo$Heating.System)

# Suppose a specific record (e.g., row 1) had an updated maintenance cost:
db_demo$Annual.Maintenance.EUR[1] <- 1550

# D) Converting character columns into categorical Factors with ordered levels:
db_demo$Energy.Class <- factor(db_demo$Energy.Class,
                               levels = c("A", "B", "C", "D", "E"),
                               ordered = TRUE)
levels(db_demo$Energy.Class)

# ------------------------------------------------------------------------------
# 3.4 Deleting Data (Removing Rows and Columns) & Adding Records
# ------------------------------------------------------------------------------

# A) Deleting a column:
# In Base R, assign NULL to a column to delete it from the data frame:
db_demo$Green.Home <- NULL

# In dplyr, use select() with a minus sign:
db_demo <- db_demo %>% select(-Price.per.sqm)

# B) Deleting specific rows by row index:
# Remove the first 3 rows from a temporary copy:
db_less_rows <- db_demo[-c(1, 2, 3), ]
nrow(db_less_rows)

# C) Deleting rows based on a condition:
# For instance, suppose we want to exclude properties older than 45 years:
db_under_45yr <- db_demo[db_demo$Building.Age.yr <= 45, ]
cat("Removed", nrow(db_demo) - nrow(db_under_45yr), "properties older than 45 years.\n")

# D) Adding a new row with rbind() and a new column with cbind():
new_property <- db_demo[1, ]  # Copy structure of 1st row
new_property$City.Zone <- "Center"
new_property$Living.Area.sqm <- 140.0
new_property$Market.Price.kEUR <- 495.0
db_with_new_row <- rbind(db_demo, new_property)
nrow(db_with_new_row)

# ------------------------------------------------------------------------------
# 3.5 Grouped Aggregations (Summary Statistics by Category)
# ------------------------------------------------------------------------------
# Compute average price and average living area for each Building.Type:

# Base R approach using aggregate():
aggregate(cbind(Market.Price.kEUR, Living.Area.sqm) ~ Building.Type,
          data = db_demo,
          FUN = mean)

# dplyr approach using group_by() and summarise():
zone_summary <- db_demo %>%
  group_by(City.Zone) %>%
  summarise(
    Count          = n(),
    Mean_Price_kEUR = round(mean(Market.Price.kEUR), 1),
    SD_Price_kEUR   = round(sd(Market.Price.kEUR), 1),
    Median_price = round(median(Market.Price.kEUR), 1)
  )
print(zone_summary)

# ------------------------------------------------------------------------------
# 3.6 Two-Way Contingency Tables & Testing Categorical Dependencies
# ------------------------------------------------------------------------------
# When working with two qualitative (categorical) variables, we use two-way
# contingency tables (cross-tabulations) to inspect their joint distribution
# and test whether they are statistically independent or associated.

# --- EXAMPLE 1: Building.Type vs Energy.Class (Strong Dependence) ---

# 1. Create the two-way frequency table (absolute counts)
tab_type_energy <- table(Building_Type = db_demo$Building.Type,
                         Energy_Class  = db_demo$Energy.Class)
print(tab_type_energy)

# 2. Add row and column marginal totals
addmargins(tab_type_energy)

# 3. Compute relative frequencies (proportions / percentages):
# Overall joint percentages (sum of all cells = 100%):
round(prop.table(tab_type_energy) * 100, 1)

# Row-conditional percentages (margin = 1 -> each row sums to 100%):
# Useful to see the distribution of Energy.Class within each Building.Type!
row_pct <- round(prop.table(tab_type_energy, margin = 1) * 100, 1)
print(row_pct)
# Interpretation: Notice that Villas (38.0% Class A, 29.6% Class B) and
# Penthouses (35.1% Class A, 31.2% Class B) have a much higher share of top
# energy ratings than Apartments (only 12.0% Class A).

# Column-conditional percentages (margin = 2 -> each column sums to 100%):
round(prop.table(tab_type_energy, margin = 2) * 100, 1)

# 4. Statistical Test of Independence: Pearson's Chi-squared Test
#   - Null Hypothesis (H0): Building.Type and Energy.Class are independent.
#   - Alternative Hypothesis (H1): There is a dependency between the two variables.
chi_test_1 <- chisq.test(tab_type_energy)
print(chi_test_1)
# Look at expected counts under H0 (all > 5, so Chi-squared approximation holds):
round(chi_test_1$expected, 1)
# Conclusion: X-squared = 172.14, df = 12, p-value < 2.2e-16.
# Since p-value < 0.05, we strongly reject H0 and conclude that Energy.Class
# depends significantly on Building.Type.

# 5. Visualizing the Two-Way Contingency Table (Grouped Barplot)
par(mfrow = c(1, 1))
barplot(t(row_pct),
        beside = TRUE,
        col = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
        legend.text = colnames(row_pct),
        args.legend = list(title = "Energy Class", x = "topright", cex = 0.85),
        xlab = "Building Type",
        ylab = "Percentage within Building Type (%)",
        main = "Distribution of Energy Class by Building Type")

# --- EXAMPLE 2: City.Zone vs Heating.System (Statistical Independence) ---
tab_zone_heating <- table(City_Zone = db_demo$City.Zone,
                          Heating   = db_demo$Heating.System)
addmargins(tab_zone_heating)

chi_test_2 <- chisq.test(tab_zone_heating)
print(chi_test_2)
# Conclusion: X-squared = 12.388, df = 8, p-value = 0.1347.
# Since p-value > 0.05, we fail to reject H0: there is no statistically
# significant dependency between the geographical City.Zone and Heating.System.

# Clean up temporary demo objects before starting the official Toolkit workflow
rm(db_demo, villas_north, central_apartments, db_sorted_base, db_sorted_dplyr,
   db_less_rows, db_under_45yr, new_property, db_with_new_row, zone_summary,
   tab_type_energy, row_pct, chi_test_1, tab_zone_heating, chi_test_2)


# ==============================================================================
# PART II: OFFICIAL LINEAR REGRESSION TOOLKIT WORKFLOW
# ==============================================================================

# ==============================================================================
# SECTION 4: DATA PRE-PROCESSING (CLEANING AND PREPARATION)
# ==============================================================================
# We now return to our raw 'dataset' loaded in Section 2 to perform the formal
# pre-processing pipeline described in the Toolkit.

# 4.1 Check for Missing Values (NAs)
# Count the total number of missing values in the entire dataset:
sum(is.na(dataset))             # Returns: [1] 5

# Count missing values column by column:
colSums(is.na(dataset))

# Identify which row numbers contain at least one NA:
which(!complete.cases(dataset)) # Note that Row 5 is incomplete (visible in head(dataset))

# Remove all rows containing missing values:
dataset_clean <- na.omit(dataset)

# (Imputation of missing data) 

# 4.2 Remove Redundant Columns (Avoiding Exact Multicollinearity)
# The column 'Living.Area.sqft' (square feet) is an exact linear multiple of
# 'Living.Area.sqm' (1 sqm = 10.7639 sqft). Keeping both in a regression model
# would introduce severe collinearity without adding any new information.
dataset_clean <- dataset_clean %>% select(-c(Living.Area.sqft))

# 4.3 Check and Remove Duplicate Rows
sum(duplicated(dataset_clean))  # Number of duplicated rows (6)
dataset_clean <- dataset_clean[!duplicated(dataset_clean), ]

# Verify final cleaned dimensions: 1200 rows and 11 columns
dim(dataset_clean)

# Rename the cleaned dataset for the regression analysis
db_house <- dataset_clean

# Select only the quantitative (numeric) features
qt_var <- db_house %>% select_if(is.numeric)
colnames(qt_var)


# ==============================================================================
# SECTION 5: EXPLORATORY DATA ANALYSIS (EDA) & STANDARDIZATION
# ==============================================================================

# 5.1 Inspecting the Distribution of the Response Variable
# Before standardizing, let us plot the histogram of our response variable
# 'Market.Price.kEUR' to verify that its distribution is unimodal and symmetric
# (approximately normal), which helps satisfy linear regression assumptions.
par(mfrow = c(1, 1))
hist(db_house$Market.Price.kEUR,
     col = "orange",
     border = "black",
     main = "Distribution of Property Market Price",
     xlab = "Market Price (kEUR)",
     ylab = "Frequency",
     breaks = 10)
# Note: If the response variable were heavily skewed, we could normalize it
# using bestNormalize() from library(bestNormalize).

# 5.2 Pairwise Scatterplots, Histograms, and Pearson Correlations
# pairs.panels() from the 'psych' package displays:
#   - Lower triangle: pairwise scatterplots with linear trend lines and ellipses
#   - Diagonal: histograms and kernel density estimates for each variable
#   - Upper triangle: Pearson linear correlation coefficients (r)
pairs.panels(db_house[, colnames(qt_var)],
             method = "pearson",   # Linear correlation coefficient
             hist.col = "red",     # Histogram bar color
             density = TRUE,       # Overlay kernel density curves
             ellipses = TRUE,      # Draw correlation ellipses
             col = "skyblue",
             smoother = TRUE,
             #pch = ".",
             col.smooth = "green")      


# 5.3 Standardization of Numerical Variables (Z-score scaling)
# Standardization transforms each quantitative variable to have mean = 0 and
# standard deviation = 1:  Z = (X - mean(X)) / sd(X).
# This allows direct comparison of regression coefficients (standardized beta
# weights) across predictors measured in different units (sqm, km, years, EUR).
db_house <- as.data.frame(scale(db_house[, colnames(qt_var)]))
head(db_house)

# 5.4 Correlation Matrix Plot
# Compute the Pearson correlation matrix and visualize it via corrplot()
corr_matrix <- cor(db_house[, colnames(qt_var)], use = "complete.obs")
print(round(corr_matrix, 2))

corrplot(corr_matrix,
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45)


# ==============================================================================
# SECTION 6: SIMPLE LINEAR REGRESSION (SLR) & ASSUMPTION DIAGNOSTICS
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1 First Simple Linear Regression: Market.Price.kEUR ~ Living.Area.sqm
# ------------------------------------------------------------------------------
# We model the response variable 'Market.Price.kEUR' as a linear function of the
# strongest single predictor, 'Living.Area.sqm' (r = 0.85).
# Mathematical model:  Y = beta_0 + beta_1 * X + epsilon

model_SLR <- lm(Market.Price.kEUR ~ Living.Area.sqm, data = db_house)

# Display detailed regression output (residuals, coefficients, t-tests, R^2, F-test)
summary(model_SLR)

# Extract model coefficients and 95% confidence intervals
coef(model_SLR)
confint(model_SLR, level = 0.95) # IC_(1-a) = b_j pm t_{1 - a/2, n - p - 1 X SE(b_j)

# Scatter plot with the fitted Simple Linear Regression line (in red)
ggplot(db_house, aes(x = Living.Area.sqm, y = Market.Price.kEUR)) +
  geom_point(size = 1.6, alpha = 0.85) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Simple Linear Regression: Market Price vs. Living Area",
       x = "Living Area (sqm, standardized)",
       y = "Market Price (kEUR, standardized)")

# 6.2 Checking Linear Regression Assumptions (Diagnostic Plots for model_SLR)
# The plot() method on an 'lm' object produces 4 fundamental diagnostic graphs:
#   1. Residuals vs Fitted: Checks linearity (red curve should be flat near 0)
#      and homoscedasticity (vertical spread should be roughly constant).
#   2. Normal Q-Q: Checks whether standardized residuals follow a Normal
#      distribution (points should lie along the dashed diagonal line).
#   3. Scale-Location: Plots sqrt(|standardized residuals|) vs Fitted values to
#      detect heteroscedasticity (non-constant variance). Notice here a mild
#      upward trend at high fitted values, because large houses vary more in
#      price depending on neighborhood quality (which is omitted in this SLR!).
#   4. Residuals vs Leverage: Identifies high-leverage points and influential
#      outliers via Cook's distance contours.
par(mfrow = c(2, 2))
plot(model_SLR, sub = "")

# ------------------------------------------------------------------------------
# 6.3 Second Simple Linear Regression: Market.Price.kEUR ~ Neighborhood.Score
# ------------------------------------------------------------------------------
# We also inspect the simple linear regression between Market.Price.kEUR and
# Neighborhood.Score (r = 0.62).

model_SLR_new <- lm(Market.Price.kEUR ~ Neighborhood.Score, data = db_house)

# View the summary of the second SLR model
summary(model_SLR_new)

# Scatter plot with the fitted regression line (in blue)
ggplot(db_house, aes(x = Neighborhood.Score, y = Market.Price.kEUR)) +
  geom_point(size = 1.6, alpha = 0.85) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simple Linear Regression: Market Price vs. Neighborhood Score",
       x = "Neighborhood Score (standardized)",
       y = "Market Price (kEUR, standardized)")

# Diagnostic plots for model_SLR_new
par(mfrow = c(2, 2))
plot(model_SLR_new, sub = "")
# Notice how the diagnostic plots confirm linearity and normality, although a
# single location predictor alone explains a smaller share of variance
# (R-squared = 0.3904) compared to Living.Area.sqm (R-squared = 0.7181).
# Combining all predictors into a Multiple Linear Regression model will capture
# both physical size and location effects simultaneously!


# ==============================================================================
# SECTION 7: MULTIPLE LINEAR REGRESSION (MLR)
# ==============================================================================
# We now extend the model to include all quantitative predictors simultaneously:
#   Market.Price.kEUR = beta_0 + beta_1 * Living.Area.sqm
#                              + beta_2 * Neighborhood.Score
#                              + beta_3 * Distance.Center.km
#                              + beta_4 * Building.Age.yr
#                              + beta_5 * Garage.Area.sqm
#                              + beta_6 * Annual.Maintenance.EUR + epsilon

model_MLR <- lm(Market.Price.kEUR ~ Living.Area.sqm + Neighborhood.Score +
                  Distance.Center.km + Building.Age.yr +
                  Garage.Area.sqm + Annual.Maintenance.EUR,
                data = db_house)

# Display the Multiple Linear Regression summary
summary(model_MLR)

# Key Interpretations from summary(model_MLR):
# - Living.Area.sqm (0.7027, p < 2e-16 ***): Strongest positive predictor.
# - Neighborhood.Score (0.3611, p < 2e-16 ***): Strong positive effect.
# - Distance.Center.km (-0.1273, p < 2e-16 ***): Significant negative effect
#   (properties farther from the city center have lower prices, all else equal).
# - Building.Age.yr (-0.0170, p = 0.0187 *): Marginally significant negative
#   effect (older buildings sell for slightly less when controlling for size/zone).
# - Garage.Area.sqm (0.0790, p = 2.28e-15 ***): Significant positive effect.
# - Annual.Maintenance.EUR (-0.0021, p = 0.7854): NOT statistically significant!
#   Even though Annual.Maintenance.EUR correlated positively with price in the
#   bivariate EDA (r = 0.46), that correlation was confounded by Living.Area.sqm
#   (larger homes have higher condo fees, r = 0.53). Once Living.Area.sqm is
#   controlled for, maintenance fees have no independent effect on market price.
# - Multiple R-squared jumps to 0.9490 (Adjusted R-squared = 0.9488), explaining
#   94.9% of the variance in Market.Price.kEUR!

# 7.1 Model Diagnostics for Multiple Linear Regression
par(mfrow = c(2, 2))
plot(model_MLR, sub = "")

# 7.2 Checking for Multicollinearity via Variance Inflation Factor (VIF)
# VIF measures how much the variance of an estimated regression coefficient is
# inflated due to collinearity among predictors.
# Rule of thumb: VIF < 5 indicates no problematic multicollinearity.
vif(model_MLR)
# All VIF values are between 1.22 and 2.53, confirming the absence of harmful
# multicollinearity after removing the redundant 'Living.Area.sqft' variable.

# 7.3 Formal Model Comparison (SLR vs MLR) using ANOVA F-test
anova(model_SLR, model_MLR)


# ==============================================================================
# SECTION 8: TESTING FOR INTERACTION EFFECTS IN MULTIPLE LINEAR REGRESSION
# ==============================================================================
# In real estate economics, the marginal value of an extra square meter of
# living space ('Living.Area.sqm') is often higher in prestigious, well-serviced
# neighborhoods ('Neighborhood.Score') than in less desirable areas.
# To test whether the effect of Living.Area.sqm on Market.Price.kEUR depends on
# the level of Neighborhood.Score, we include their interaction term using '*'.
# Note: In R formulas, 'A * B' automatically expands to 'A + B + A:B' (both main
# effects plus their pairwise interaction term).

model_MLR_int <- lm(Market.Price.kEUR ~ Living.Area.sqm + Neighborhood.Score +
                      Distance.Center.km + Neighborhood.Score*Distance.Center.km +
                      Building.Age.yr + Garage.Area.sqm,
                    data = db_house)

# Display the summary of the Interacting Model
summary(model_MLR_int)

# Key Findings from summary(model_MLR_int):
# - The interaction term 'Living.Area.sqm:Neighborhood.Score' has a positive
#   coefficient (0.087797) and is highly statistically significant (p < 2e-16 ***).
# - This confirms a synergistic effect: larger living areas command an even
#   greater price premium when located in high-scoring neighborhoods.
# - Multiple R-squared increases further to 0.9565 (Adjusted R-squared = 0.9563),
#   and the Residual Standard Error drops from 0.2263 to 0.2091.

# Compare the additive MLR model and the interaction MLR model via ANOVA and AIC:
anova(model_MLR, model_MLR_int)
AIC(model_SLR, model_MLR, model_MLR_int)

# Diagnostic plots for the interaction model
par(mfrow = c(2, 2))
plot(model_MLR_int, sub = "")
par(mfrow = c(1, 1))

# 8.1 Making Predictions with the Fitted Model
# Predict standardized market prices (with 95% prediction intervals) for the
# first 5 observations in the dataset:
predicted_values <- predict(model_MLR_int,
                            newdata = db_house[1:5, ],
                            interval = "prediction",
                            level = 0.95)
cbind(Observed = db_house$Market.Price.kEUR[1:5], round(predicted_values, 3))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
