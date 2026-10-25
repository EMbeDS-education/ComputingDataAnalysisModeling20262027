# Install the necessary packages
install.packages("haven") # Import file in different format (i.e.,Excel,Stata,SAS)
install.packages("ggplot2")    # For plotting
install.packages("dplyr")      # For data manipulation
install.packages("summarytools") # For data overview
install.packages("corrplot")    # For data correlation
install.packages("car") # For regression diagnostics
install.packages("tidyverse") # Data processing
install.packages("psych") # Pairing variables

# Load the necessary libraries
library(haven)
library(ggplot2)
library(dplyr)
library(summarytools)
library(corrplot)
library(car)
library(tidyverse)
library(psych)

setwd("C:/Users/Utente/OneDrive - Scuola Superiore Sant'Anna/Laboratorio_Stat_Appl/Practicum1")

# Load the dataset
dataset <- read_dta("mensa_esercitazione.dta")

# View the first few rows of the dataset and its dimension RxC
head(dataset)

# View the "dimensions" of data in terms of rows and columns
dim(dataset)

# Show the internal structure of each column
str(dataset)

# Check for missing values in the dataset
sum(is.na(dataset))

# Show the summary of each column and look at missing data for each variable
summary(dataset)


#Look at variables for which you have missing data
table(dataset$code_birth_place[is.na(dataset$zip_birth_place)])

#Create useful vars
dataset$italian[!is.na(dataset$zip_birth_place)]<-0
dataset$italian[is.na(dataset$zip_birth_place)]<-1

#Define factor variables
dataset$canteen_code <- factor(dataset$canteen_code,
                                     levels = c(1,2,3,4,5),
                                     labels = c("Canteen1", "Canteen2", "Canteen3","Canteen4","Canteen5"))  

dataset$meal_type <- factor(dataset$meal_type,
                                  levels = c(1,2,3,4),
                                  labels = c("Light meal 1", "Light meal 2", "Pizza/Sandwich","Full meal"))  

dataset$on_track <- factor(dataset$on_track,
                                levels = c(1,2),
                                labels = c("On track", "Not on track"))  

dataset$course <- factor(dataset$course,
                               levels = c(1,2,4,5),
                               labels = c("Bachelor", "Master degree", "Degree", "PhD"))  

dataset$user_type <- factor(dataset$user_type,
                         levels = c(1,2),
                         labels = c("Student", "Foreign"))  

dataset$italian <- factor(dataset$italian,
                                levels = c(1,0),
                                labels = c("Italian", "Not italian"))  


# Removing and checking duplicate rows
dataset_clean <- dataset[!duplicated(dataset), ]

dataset_clean <- dataset_clean %>%
  group_by(ID, meal_year) %>%
  mutate(count_year=n()) %>%
  ungroup()

# Remove columns that are not useful or redundant
dataset_clean <- dataset_clean %>% select(-c(code_birth_place,zip_birth_place))

# Remove rows with missing values if appropriate!
dataset_clean <- na.omit(dataset_clean)

# Rename the dataset
db_mensa <- dataset_clean

#Look at data about one year, i.e. 2014
db_mensa2014<- db_mensa  %>% 
  filter(meal_year==2014) 

#Obtain median calories over accesses 
db_mensa2014 <- db_mensa2014 %>%
  group_by(ID) %>%
  mutate(calories_median=median(calories)) %>%
  ungroup()

#Select single record
db_mensa2014SR <- db_mensa2014 %>%
  distinct(ID, meal_year, .keep_all = TRUE)  

# Remove columns that are not useful or redundant
db_mensa2014SR <- db_mensa2014SR %>% select(-c(meal_date,meal_hour,ID,calories,meal_year))

# Select numeric features to scale data and plot correlation distribution
qt_var <- db_mensa2014SR[c("registration_year", "age", "calories_median")]

# Standardization of numeric variables
db_mensa2014SR <- as.data.frame(scale(db_mensa2014SR[, colnames(qt_var)]))

# Check if everything is ok
head(db_mensa2014SR)

# scatter plot for quantitative variables
pairsplot<-pairs.panels(db_mensa2014SR[,colnames(qt_var)],
             method = "pearson", # Correlation function
             hist.col = "red",
             density = TRUE, # Show density plots
             ellipses = TRUE # Correletion allipses
)

# save the graph
ggsave("pairsplot.png",scale=0.7, plot = pairsplot,height=6.27,width=6.15, units="in")
