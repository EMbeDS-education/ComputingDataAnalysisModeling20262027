# ==============================================================================
# APPLIED STATISTICAL MODELLING (A.A. 2026-2027)
# Lab 2: Generalized Linear Models (GLMs) in R
#
# Author: Claudio Mazzi
#         Department of Computer Science, University of Pisa
#         MeS Laboratory, Sant'Anna School for Advanced Studies, Pisa
#         Email: claudio.mazzi@santannapisa.it
#
# Description:
# This script accompanies the Lecture Toolkit on Generalized Linear Models.
# It covers the practical implementation in R of:
#   1. Motivation: Why Linear Regression Fails on Binary Outcomes (Simulation)
#   2. Binary GLM: Logistic (Logit) and Probit Models (heart.csv)
#   3. Ordinal Logistic Regression: Cumulative Logit & Brant Test (Customer_Satisfaction.csv)
#   4. Multinomial Logistic Regression: Discrete Choice & Relative Risk Ratios (Travel_Mode_Choice.csv)
# ==============================================================================


# ==============================================================================
# SECTION 1: PACKAGES INSTALLATION AND LOADING
# ==============================================================================

# Required libraries:
required_packages <- c(
  "ggplot2",     # Advanced plotting
  "dplyr",       # Data manipulation
  "tidyr",       # Data reshaping
  "pROC",        # ROC curves and AUC
  "pscl",        # McFadden's pseudo-R2 (pR2)
  "MASS",        # Ordinal logistic regression (polr)
  "nnet",        # Multinomial logistic regression (multinom)
  "brant",       # Brant test for parallel regression assumption
  "GGally",      # Pairwise scatter and correlation matrices
  "ggcorrplot"   # Correlation heatmap plotting
)

# Install missing packages automatically
missing_pkgs <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(pROC)
library(pscl)
library(MASS)
library(nnet)
library(brant)
library(GGally)
library(ggcorrplot)


# ==============================================================================
# SECTION 2: WHY LINEAR REGRESSION FAILS ON BINARY OUTCOMES
# ==============================================================================
# When the response variable Y is binary (0 or 1), fitting an Ordinary Least
# Squares (OLS) linear model (Linear Probability Model, LPM) leads to:
#   1. Nonsensical predictions: predicted values can be < 0 or > 1.
#   2. Heteroscedastic residuals: Var(Y|X) = p(X)*(1 - p(X)), which varies with X.
#   3. Non-normal errors: residuals can only take two values, violating normality.

set.seed(123)
N_sim <- 100
X_sim <- rnorm(N_sim, mean = 5, sd = 2)

# True underlying probability follows a logistic curve
prob_sim <- 1 / (1 + exp(-0.5 * (X_sim - 5)))
Outcome_sim <- rbinom(N_sim, size = 1, prob = prob_sim)
data_sim <- data.frame(Outcome = Outcome_sim, X = X_sim)

# Display first few rows
head(data_sim)

# Fit Ordinary Linear Regression (LPM)
linear_model_sim <- lm(Outcome ~ X, data = data_sim)
summary(linear_model_sim)

# Plot Linear Fit vs Logistic Fit
ggplot(data_sim, aes(x = X, y = Outcome)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  stat_smooth(method = "glm", method.args = list(family = binomial(link = "logit")), 
              color = "darkgreen", se = FALSE, size = 1.2) +
  labs(title = "Linear Probability Model (Red) vs. Logistic Regression (Green)",
       x = "Predictor X", y = "Probability of Outcome = 1") +
  theme_minimal()

# Fit Logistic Regression via glm()
logistic_model_sim <- glm(Outcome ~ X, data = data_sim, family = binomial(link = "logit"))
summary(logistic_model_sim)


# ==============================================================================
# SECTION 3: BINARY LOGIT AND PROBIT REGRESSION
# DATASET: heart.csv (Cleveland & Hungarian Heart Disease Dataset)
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "heart.csv" contains clinical and diagnostic observations for 918
# adult patients evaluated for cardiovascular disease across 12 variables:
#
# Binary Response Variable:
#   1. HeartDisease     : Presence of significant coronary heart disease
#                         (1 = Disease present [angiographic status > 50% stenosis],
#                          0 = Normal / Absence of heart disease)
#
# Categorical Predictors:
#   2. Sex              : Biological sex of the patient (M = Male, F = Female)
#   3. ChestPainType    : Type of reported chest pain:
#                         - TA: Typical Angina (substernal pain provoked by exertion)
#                         - ATA: Atypical Angina (chest pain with atypical features)
#                         - NAP: Non-Anginal Pain (non-cardiac discomfort)
#                         - ASY: Asymptomatic (no pain, often high risk)
#   4. RestingECG       : Resting electrocardiographic results:
#                         - Normal: Normal ECG reading
#                         - ST: ST-T wave abnormality (T wave inversions or ST > 0.05 mV)
#                         - LVH: Probable or definite left ventricular hypertrophy
#   5. ExerciseAngina   : Exercise-induced angina pectoris (Y = Yes, N = No)
#   6. ST_Slope         : The slope of the peak exercise ST segment:
#                         - Up: Upsloping (normal physiologic response)
#                         - Flat: Flat / horizontal (classic myocardial ischemia)
#                         - Down: Downsloping (severe ischemic indicator)
#
# Quantitative Predictors:
#   7. Age              : Age of the patient in years (scale: 28 - 77)
#   8. RestingBP        : Resting systolic blood pressure on admission in mm Hg (80 - 200)
#   9. Cholesterol      : Serum cholesterol level in mg/dL (0 - 603)
#  10. FastingBS        : Fasting blood sugar indicator:
#                         (1 = Fasting Blood Sugar > 120 mg/dL [diabetic risk],
#                          0 = Fasting Blood Sugar <= 120 mg/dL)
#  11. MaxHR            : Maximum heart rate achieved during stress test in bpm (60 - 202)
#  12. Oldpeak          : ST depression induced by exercise relative to rest in mm (-2.6 - 6.2)
# ==============================================================================

# 3.1 Load Dataset (using relative path from current directory or toolkit folder)
if (file.exists("heart.csv")) {
  data_heart <- read.csv("heart.csv")
} else if (file.exists("2_GLM/heart.csv")) {
  data_heart <- read.csv("2_GLM/heart.csv")
} else {
  data_heart <- read.csv("2_Generalized_Linear_Model_toolkit/heart.csv")
}

cat("=== Original Data Structure ===\n")
str(data_heart)
head(data_heart)

# 3.2 Data Pre-processing: Binary & One-Hot Encoding
# Binary encode Sex and ExerciseAngina
data_heart$Sex <- ifelse(data_heart$Sex == "M", 1, 0)
data_heart$ExerciseAngina <- ifelse(data_heart$ExerciseAngina == "Y", 1, 0)

# One-hot encode multi-level categorical factors
data_heart <- cbind(
  data_heart, 
  model.matrix(~ ChestPainType + RestingECG + ST_Slope - 1, data = data_heart)
)

# Remove the original categorical columns
data_heart$ChestPainType <- NULL
data_heart$RestingECG <- NULL
data_heart$ST_Slope <- NULL

cat("=== Pre-processed Data Structure ===\n")
str(data_heart)

# 3.3 Fitting the Logistic Regression (Logit) Model
logit_model <- glm(
  HeartDisease ~ Age + Sex + RestingBP + Cholesterol + FastingBS + 
    MaxHR + ExerciseAngina + Oldpeak + ChestPainTypeATA + ChestPainTypeNAP + 
    ChestPainTypeTA + RestingECGST + RestingECGNormal + ST_SlopeFlat + ST_SlopeUp, 
  family = binomial(link = "logit"), 
  data = data_heart
)

cat("=== Logistic Regression Summary ===\n")
summary(logit_model)

# Odds Ratios and 95% Confidence Intervals
cat("=== Odds Ratios (exp(beta)) ===\n")
odds_ratios_logit <- exp(coef(logit_model))
print(round(odds_ratios_logit, 4))

# 3.4 Fitting the Probit Regression Model
# The probit link uses the standard normal cumulative distribution function: Phi^(-1)(p) = X*beta
probit_model <- glm(
  HeartDisease ~ Age + Sex + RestingBP + Cholesterol + FastingBS + 
    MaxHR + ExerciseAngina + Oldpeak + ChestPainTypeATA + ChestPainTypeNAP + 
    ChestPainTypeTA + RestingECGST + RestingECGNormal + ST_SlopeFlat + ST_SlopeUp, 
  family = binomial(link = "probit"), 
  data = data_heart
)

cat("=== Probit Regression Summary ===\n")
summary(probit_model)

# 3.5 Model Comparison: Pseudo-R2, AIC, BIC
cat("=== Goodness of Fit Metrics ===\n")
cat("Logit  McFadden R2:", pscl::pR2(logit_model)["McFadden"], 
    "AIC:", AIC(logit_model), "BIC:", BIC(logit_model), "\n")
cat("Probit McFadden R2:", pscl::pR2(probit_model)["McFadden"], 
    "AIC:", AIC(probit_model), "BIC:", BIC(probit_model), "\n")

# 3.6 Classification Performance & Confusion Matrix (Threshold = 0.5)
pred_prob_logit  <- predict(logit_model, type = "response")
pred_prob_probit <- predict(probit_model, type = "response")

pred_class_logit  <- ifelse(pred_prob_logit > 0.5, 1, 0)
pred_class_probit <- ifelse(pred_prob_probit > 0.5, 1, 0)

cm_logit  <- table(Predicted = pred_class_logit, Actual = data_heart$HeartDisease)
cm_probit <- table(Predicted = pred_class_probit, Actual = data_heart$HeartDisease)

cat("=== Confusion Matrix (Logit) ===\n")
print(cm_logit)
cat("Logit Accuracy:", sum(diag(cm_logit)) / sum(cm_logit), "\n")

cat("=== Confusion Matrix (Probit) ===\n")
print(cm_probit)
cat("Probit Accuracy:", sum(diag(cm_probit)) / sum(cm_probit), "\n")

# 3.7 ROC Curve and Area Under the Curve (AUC)
roc_logit <- roc(data_heart$HeartDisease, pred_prob_logit)
roc_probit <- roc(data_heart$HeartDisease, pred_prob_probit)

cat("Logit  AUC:", auc(roc_logit), "\n")
cat("Probit AUC:", auc(roc_probit), "\n")

plot(roc_logit, col = "blue", lwd = 2, main = "ROC Curve: Logit vs Probit")
plot(roc_probit, col = "red", lty = 2, lwd = 2, add = TRUE)
legend("bottomright", legend = c(paste("Logit AUC =", round(auc(roc_logit), 4)),
                                 paste("Probit AUC =", round(auc(roc_probit), 4))),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


# ==============================================================================
# SECTION 4: ORDINAL LOGISTIC REGRESSION (POLR)
# DATASET: Customer_Satisfaction.csv (Airline Passenger Survey)
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "Customer_Satisfaction.csv" evaluates overall airline passenger
# satisfaction across 1,200 flights with balanced representation across 7 variables:
#
# Ordinal Response Variable:
#   1. Satisfaction     : Overall passenger post-flight rating tier (ordered factor):
#                         - Low    : Dissatisfied / poor flight experience (n = 400)
#                         - Medium : Neutral / acceptable experience (n = 400)
#                         - High   : Highly satisfied / outstanding experience (n = 400)
#                         --> Ordered hierarchy: Low < Medium < High
#
# Categorical Predictor:
#   2. Travel_Type      : Flight purpose / passenger segment:
#                         - Business : Commercial / corporate travel (reference baseline)
#                         - Leisure  : Vacation / private tourist travel
#
# Quantitative Predictors:
#   3. Seat_Comfort     : Passenger seat comfort rating (Likert scale: 1 = very poor, 5 = excellent)
#   4. Cabin_Service    : Flight attendants and in-flight service rating (Likert: 1 - 5)
#   5. Food_Quality     : In-flight catering satisfaction rating (Likert: 1 - 5)
#   6. Flight_Delay_min : Total departure and arrival flight delay in minutes (scale: 0 - 240)
#   7. Age              : Passenger age in years (scale: 18 - 80)
# ==============================================================================

# 4.1 Load Dataset
if (file.exists("Customer_Satisfaction.csv")) {
  df_ord <- read.csv("Customer_Satisfaction.csv")
} else if (file.exists("2_GLM/Customer_Satisfaction.csv")) {
  df_ord <- read.csv("2_GLM/Customer_Satisfaction.csv")
} else {
  df_ord <- read.csv("2_Generalized_Linear_Model_toolkit/Customer_Satisfaction.csv")
}

str(df_ord)
head(df_ord)

# 4.2 Define Ordered Factor
df_ord$Satisfaction <- factor(df_ord$Satisfaction, levels = c("Low", "Medium", "High"), ordered = TRUE)
table(df_ord$Satisfaction)

# 4.3 Exploratory Data Analysis (EDA)
# Kendall rank correlation across numeric features
numeric_cols <- c("Seat_Comfort", "Cabin_Service", "Food_Quality", "Flight_Delay_min", "Age")
cor_kendall <- cor(df_ord[, numeric_cols], method = "kendall")
ggcorrplot(cor_kendall, type = "lower", lab = TRUE, title = "Kendall Rank Correlation Matrix")

# Class distribution histogram
ggplot(df_ord, aes(x = Satisfaction, fill = Satisfaction)) +
  geom_bar(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("Low" = "#E64B35", "Medium" = "#4DBBD5", "High" = "#00A087")) +
  labs(title = "Airline Passenger Satisfaction Distribution (Balanced)",
       x = "Satisfaction Tier", y = "Passenger Count") +
  theme_minimal()

# 4.4 Fit Ordinal Logistic Regression Model (MASS::polr)
# Cumulative logit link: logit(P(Y <= j)) = alpha_j - X*beta
ordinal_model <- polr(
  Satisfaction ~ Seat_Comfort + Cabin_Service + Food_Quality + Flight_Delay_min + Travel_Type + Age, 
  data = df_ord, 
  Hess = TRUE
)

cat("=== Ordinal Logistic Regression Summary ===\n")
summary(ordinal_model)

# Exponentiated coefficients (Odds Ratios)
cat("=== Proportional Odds Ratios (exp(beta)) ===\n")
print(round(exp(coef(ordinal_model)), 4))

# 4.5 Likelihood Ratio Test vs. Null Model
null_ordinal_model <- polr(Satisfaction ~ 1, data = df_ord, Hess = TRUE)
cat("=== Likelihood Ratio Test (Null vs. Full) ===\n")
print(anova(null_ordinal_model, ordinal_model))

# 4.6 Classification Performance & Confusion Matrix
pred_ord_class <- predict(ordinal_model, type = "class")
cm_ord <- table(Predicted = pred_ord_class, Actual = df_ord$Satisfaction)

cat("=== Confusion Matrix (Ordinal Logit) ===\n")
print(cm_ord)
accuracy_ord <- sum(diag(cm_ord)) / sum(cm_ord)
cat("Ordinal Model Accuracy:", round(accuracy_ord * 100, 2), "%\n")

# 4.7 Testing the Proportional Odds Assumption (Brant Test)
# H0: The regression coefficients are equal across all response cutpoints (slopes are parallel)
# A p-value > 0.05 indicates that the proportional odds assumption holds!
cat("=== Brant Test for Parallel Regression Assumption ===\n")
brant_test_res <- brant(ordinal_model)
print(brant_test_res)


# ==============================================================================
# SECTION 5: MULTINOMIAL LOGISTIC REGRESSION (MULTINOM)
# DATASET: Travel_Mode_Choice.csv (Commuter Mode Choice - McFadden Discrete Choice)
# ==============================================================================
#
# DATASET DESCRIPTION:
# The dataset "Travel_Mode_Choice.csv" records commuter transportation decisions
# in an urban metropolitan area for 1,200 individuals across 5 variables:
#
# Nominal Response Variable (Unordered Multi-Category):
#   1. Travel_Mode            : Primary daily commute mode:
#                               - Car            : Private automobile driver (n = 408)
#                                                  --> Reference / Baseline Alternative
#                               - Public_Transit : Urban subway / tram / bus commuter (n = 513)
#                               - Bicycle        : Active commuter cycling (n = 279)
#                               --> Nominal classes without natural ordering
#
# Quantitative Predictors:
#   2. Commute_Distance_km    : One-way commute distance from residence to workplace in km (0.5 - 45.0)
#   3. Monthly_Income_kEUR    : Monthly net household income in thousands of Euros (€1.2k - €12.5k)
#   4. Public_Transit_Access  : Urban transit accessibility rating at residence (scale: 1 = very poor,
#                               10 = optimal transit hub proximity with frequent lines)
#   5. Commuter_Age           : Age of commuter in years (scale: 18 - 72)
# ==============================================================================

# 5.1 Load Dataset
if (file.exists("Travel_Mode_Choice.csv")) {
  df_multi <- read.csv("Travel_Mode_Choice.csv")
} else if (file.exists("2_GLM/Travel_Mode_Choice.csv")) {
  df_multi <- read.csv("2_GLM/Travel_Mode_Choice.csv")
} else {
  df_multi <- read.csv("2_Generalized_Linear_Model_toolkit/Travel_Mode_Choice.csv")
}

str(df_multi)
head(df_multi)
table(df_multi$Travel_Mode)

# 5.2 Set Baseline Category
# In discrete choice modeling, we choose one mode as the reference (baseline).
# Here, 'Car' is designated as the reference alternative.
df_multi$Travel_Mode <- factor(df_multi$Travel_Mode)
df_multi$Travel_Mode <- relevel(df_multi$Travel_Mode, ref = "Car")

# 5.3 Fit Multinomial Logistic Regression Model (nnet::multinom)
model_multi <- multinom(
  Travel_Mode ~ Commute_Distance_km + Monthly_Income_kEUR + Public_Transit_Access + Commuter_Age, 
  data = df_multi
)

cat("=== Multinomial Model Summary ===\n")
summary(model_multi)

# 5.4 Statistical Significance: Wald z-scores and P-Values
coefs_multi <- summary(model_multi)$coefficients
se_multi    <- summary(model_multi)$standard.errors
z_multi     <- coefs_multi / se_multi
p_val_multi <- 2 * (1 - pnorm(abs(z_multi)))

cat("=== Two-Tailed P-Values ===\n")
print(round(p_val_multi, 5))

# 5.5 Relative Risk Ratios (RRR)
# RRR = exp(beta): Multiplicative effect on odds of choosing alternative k vs baseline Car
cat("=== Relative Risk Ratios (Odds Ratios vs Car) ===\n")
rrr_multi <- exp(coefs_multi)
print(round(rrr_multi, 4))

# 5.6 Predicted Probabilities and Classification
pred_probs_multi <- predict(model_multi, type = "probs")
pred_mode_multi  <- predict(model_multi, type = "class")

cm_multi <- table(Predicted = pred_mode_multi, Actual = df_multi$Travel_Mode)
cat("=== Confusion Matrix (Multinomial Logit) ===\n")
print(cm_multi)
cat("Multinomial Accuracy:", round(sum(diag(cm_multi)) / sum(cm_multi) * 100, 2), "%\n")

# 5.7 Visualizing Observed vs. Predicted Choice Frequencies
obs_counts  <- table(df_multi$Travel_Mode)
pred_counts <- table(pred_mode_multi)

df_comp <- data.frame(
  Mode = rep(names(obs_counts), 2),
  Count = c(as.numeric(obs_counts), as.numeric(pred_counts)),
  Type = rep(c("Observed", "Predicted"), each = length(obs_counts))
)

ggplot(df_comp, aes(x = Mode, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Observed" = "#2b5c8f", "Predicted" = "#d95f02")) +
  labs(title = "Observed vs. Predicted Commuter Travel Mode Counts",
       x = "Travel Mode", y = "Number of Commuters") +
  theme_minimal()

cat("\n=== GLM Analysis Script Completed Successfully ===\n")
