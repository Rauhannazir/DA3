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

### Data Munging/Cleaning ###

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



# Looking at the relationships of target variable and key predictors

# 1- grade92 & wage per hour(wph)  

ggplot(elementary_middle_school_teachers, aes(x = grade92, y = wph)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) + 
  theme_bw()

# 2- age with wage per hour (wph)

ggplot(elementary_middle_school_teachers, aes(x = age, y = wph)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) + 
  theme_bw()




## Looking at the distribution of  the target variable

ggplot(elementary_middle_school_teachers) +
  geom_density(aes(x=wph)) +
  theme_bw() + 
  labs(x = "Wage per Hour (USD)")


# Even though it has a bit of right-tail, I have decided to not take the log, as the relation between the key variables 
# is linear


# Checking the distribution of the age variable
ggplot(elementary_middle_school_teachers) +
  geom_density(aes(x=age)) +
  theme_bw() + 
  labs(x = "age")

#even though it is kind of normally distributed, will check how it varies with including the square term

#adding the age squared term

elementary_middle_school_teachers <- elementary_middle_school_teachers[, agesq := age^2]

# Checking the distribution of the age variable
ggplot(elementary_middle_school_teachers) +
  geom_density(aes(x=agesq)) +
  theme_bw() + 
  labs(x = "age squared")


### Creating factors for key variables, so that we can run more effective and interpretable regressions moving forward

# Creating levels for grade92 variable in a new variable (educ)
elementary_middle_school_teachers <- elementary_middle_school_teachers[grade92 == 43, educ := "Bachelors"]
elementary_middle_school_teachers <-elementary_middle_school_teachers[grade92 == 44, educ := "Masters"]
elementary_middle_school_teachers <-elementary_middle_school_teachers[grade92 == 45, educ := "Professional-School"]
elementary_middle_school_teachers <-elementary_middle_school_teachers[grade92 == 46, educ := "Doctorate degree"]


# Creating factor variable for marital variable as married_status

elementary_middle_school_teachers[marital <= 2, married_status := "married"]
elementary_middle_school_teachers[marital <= 6 & marital >= 3, married_status := "separated"]
elementary_middle_school_teachers[marital == 7, married_status := "never married"]

# Creating factor variable for race (Will divide into white and others)

elementary_middle_school_teachers <- elementary_middle_school_teachers[race == 1, race_dummy := "white"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[race != 1, race_dummy := "other"]


# Creating factor variable for sex as gender

elementary_middle_school_teachers[sex == 1, gender := "male"]
elementary_middle_school_teachers[sex == 2, gender := "female"]






  
  
  
  
  