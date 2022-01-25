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

#Doing the exploratory analysis on the data table I am working with 

skim(elementary_middle_school_teachers)





  
  
  
  
  