# Step 1: Install and Load Required Packages
# --------------------------------------------
install.packages("ggplot2")       # For plotting
install.packages("dplyr")         # For data manipulation
install.packages("summarytools")  # For data overview
install.packages("corrplot")      # For data correlation
install.packages("car")           # For regression diagnostics
install.packages("tidyverse")     # Data processing
install.packages("psych")         # Pairing variables
install.packages("bestNormalize") # Normalize outcome variable

library(ggplot2)
library(dplyr)
library(summarytools)
library(corrplot)
library(car)
library(tidyverse)
library(psych)
library(bestNormalize)

# Step 2: Load the Dataset
# ------------------------
# Load the dataset (adjust the file path as necessary)
# Use "/" on Mac - "\\" on Windows
db_walmart <- read.csv("/PATH_TO_DB/Walmart_Sales.csv", sep = ",")

# View the first few rows of the dataset and its dimension RxC
head(db_walmart)
dim(db_walmart)
# Show the summary of each column
summary(db_walmart)
# Show the internal structure of each column
str(db_walmart)

# Step 3: Data Cleaning and Preparation
# -------------------------------------
# Check for missing values in the dataset
sum(is.na(db_walmart))

# Remove missing values / imputation
# dataset_wnan <- na.omit(db_walmart)
# dataset_wnan <- db_walmart %>% drop_na()
# db_walmart$Temperature[is.na(db_walmart$Temperature)] <- mean(db_walmart$Temperature, na.rm = TRUE)

# Removing duplicate rows or redundant columns
dataset_clean <- db_walmart[!duplicated(db_walmart), ]
dataset_clean_1 <- subset(db_walmart, select = -c(CPI, Unemployment))
dataset_clean_2 <- db_walmart %>% select(-CPI, -Unemployment) # %>% == PIPE

summary(dataset_clean)
head(dataset_clean)

# Rename the dataset
db_walmart <- dataset_clean
str(db_walmart)

# Inspect numeric variables
qt_var <- db_walmart %>% select_if(is.numeric)
head(qt_var)
# Holiday_Flag is a dummy, exclude it from scaling
qt_var <- subset(qt_var, select = -c(Holiday_Flag, Store))

###################### SCALING #################################################
# Standardization of numeric variables
db_walmart_scaled <- as.data.frame(scale(db_walmart[, colnames(qt_var)]))

head(db_walmart_scaled)
###################### SCALING #################################################


# Step 4: Exploratory Data Analysis (EDA)
# ---------------------------------------
# scatter plot for quantitative variables
pairs.panels(db_walmart[,colnames(qt_var)],
             method = "pearson", # Correlation function
             hist.col = "red",
             density = TRUE, # Show density plots
             ellipses = TRUE, # Correlation ellipses
             )

# Correlation matrix between quant. variables
corr_matrix <- cor(db_walmart[, colnames(qt_var)], use = "complete.obs")
corrplot(corr_matrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Boxplot (use scaled db for a better visualization)
boxplot(db_walmart_scaled, main = "Box Plot", ylab = "Values", col = "lightblue")


# Step 5: Simple Linear Regression
# --------------------------------
# Checking for normal bhaviour of the response variable
hist(db_walmart$Weekly_Sales,
     main = "",
     col = "orange",
     xlab = "Weekly Sales"
     )

# Normalization
WSales_norm <- bestNormalize(db_walmart$Weekly_Sales)
db_walmart$WSales_norm <- WSales_norm$x.t

head(db_walmart)

hist(db_walmart$WSales_norm,
     main = "",
     col = "orange",
     xlab = "Normalized Weekly Sales"
)


# Model: Weekly Sales (Resp Var) as a Air Temperature
model_SLR <- lm(WSales_norm ~ Temperature, data=db_walmart)

# View the summary of the model
summary(model_SLR)

# Step 6: Checking Linear Regression Assumptions
# --------------------------------
# Plot the regression line
ggplot(db_walmart, aes(x=Temperature, y=WSales_norm)) +
  geom_point() +
  geom_smooth(method="lm", col="red", se=TRUE) +
  theme_minimal() +
  labs(title="", x="Air Temperature", y="Weekly Sales")

# Inspected the model
# Generates diagnostic plots for the linear regression
# Residual vs Fitted / Normal Q-Q / Scale-Location / Residuals vs Leverage
par(mfrow = c(2, 2)) 
plot(model_SLR, sub = "")

# Step 7: Multiple Linear Regression
# ----------------------------------
# Choose the independent variables to include -> complete set 
model_MLR <- lm(WSales_norm  ~ Temperature + Fuel_Price + 
                  CPI + Unemployment + Holiday_Flag,
                  data = db_walmart)

summary((model_MLR))


# Step 8: Model Diagnostics
# -------------------------
par(mfrow = c(2, 2)) 
plot(model_MLR, sub = "")


# Step 9: Multiple Linear Regression with interaction between predictors
# -------------------------
# Switch on the interaction between Temperature and Fuel Price
model_MLR_int <- lm(WSales_norm  ~ Temperature + Fuel_Price +  Holiday_Flag + 
                      Temperature*Fuel_Price,
                      data = db_walmart)
# Results of the Interacting Model 
summary(model_MLR_int)

par(mfrow = c(2, 2)) 
plot(model_MLR_int, sub = "")


# Dummy regression
model_dummy <- lm(WSales_norm  ~ Holiday_Flag,
                data = db_walmart)

summary(model_dummy)


# THE END
# ------------------



