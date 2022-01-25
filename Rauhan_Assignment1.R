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








  
  
  
  
  