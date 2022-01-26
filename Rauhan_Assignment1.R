rm(list= ls())

# Loading Libraries

library(data.table)
library(tidyverse)
library(modelsummary)
library(fixest)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(gridExtra)
library(ggplot2)

# Loading data
data <- fread("https://osf.io/4ay9x/download")

## Data Munging/Cleaning

#Counting the people in each occupation to check the sample size I will be working with

Count_of_occupation <- data[, .(Count = .N), by = occ2012][order(Count,decreasing=TRUE)]
#view(Count_of_occupation)

#Choosing the occupation as Elementary and Middle School teachers. One reason behind this was the sample size was 
# largest for this from the available data. This would help in building more accurate predictive models.

elementary_middle_school_teachers <- data[occ2012 == 2310]  
#view(elementary_middle_school_teachers)

#Looking at the structure of data loaded to get an idea of variables I am working with 
str(elementary_middle_school_teachers)

## Doing the exploratory analysis on the data table I am working with 

skim(elementary_middle_school_teachers)
summary(elementary_middle_school_teachers)

#Creating a new variable for wage per hour(wph), which will be the target variable 

elementary_middle_school_teachers <-  elementary_middle_school_teachers[, wph := earnwke/uhours]

# Creating data summary for the target variable and some of the important predictors
datasummary(wph + grade92 + age ~ Mean + SD + Min + Max + P25 + P75 + N , data = elementary_middle_school_teachers)


## There is not a lot of variation in the grades for this occupation, which could mean that it might not be able to
# play a key part in prediction as it does not have a lot of data points. Age on the other hand has more variation.


#Filtering the data for possible errors 

#The mean of wage is around $26, so anything less than 10 might be a error and on top of that I only want to consider
#the full time employees, so I will include a filter for that as well

elementary_middle_school_teachers <- elementary_middle_school_teachers[wph > 10]
elementary_middle_school_teachers <- elementary_middle_school_teachers[uhours >= 32] #as per the IRS website

#Another important thing that we need to do is to find out the missing values for variables

NA_filter <- sapply(elementary_middle_school_teachers, function(x) sum(is.na(x)))
NA_filter[NA_filter > 0]

# Only the ethnicity variable has NA values, and that too in large number (2970). Our total number of observations 
# after filtering the data is 3195. So with over 90% of NA values, it would not make much sense to include it in
# model as even if we wanted to include the results would not be useful.

#Dropping the Ethnicity column 

elementary_middle_school_teachers <- elementary_middle_school_teachers[, ethnic := NULL]

# Counting Number observation in each Education Level
elementary_middle_school_teachers[,.(Count=.N), by= grade92][order(Count,decreasing=TRUE)]

#Majority of the teachers are of Grade 43 & 44 (Over 95%), so I am making a decision of dropping the teachers of lower
# grades as it is highly unlikely that they get this teaching job unless they are grade 43 or above 

elementary_middle_school_teachers <- elementary_middle_school_teachers[grade92 >= 43]

# Doing the same for age variable 

elementary_middle_school_teachers[,.(Count=.N), by= age][order(Count,decreasing=TRUE)]

#There is no such need to drop any age bracket since the data is distributed evenly, which is true for this teaching 
#profession. Still dropping the single observation for below age 22. 

elementary_middle_school_teachers <- elementary_middle_school_teachers[age >= 22]










  
  
  
  
  