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

# Looking at the result
datasummary(wph*factor(educ)  ~ Mean + Percent()+ SD + Min + Max + P25 + P75 + N , data = elementary_middle_school_teachers)
#There does not seem to be much difference between the average wages of all the education levels except the ones with 
#Doctorate degrees, with a slightly higer mean wage 

# Creating factor variable for marital variable as married_status

elementary_middle_school_teachers[marital <= 2, married_status := "married"]
elementary_middle_school_teachers[marital <= 6 & marital >= 3, married_status := "separated"]
elementary_middle_school_teachers[marital == 7, married_status := "never married"]

# Looking at the result
datasummary(wph*factor(married_status)  ~ Mean + Percent()+ SD + Min + Max + P25 + P75 + N , data = elementary_middle_school_teachers)
#Marriage status does not appear to have an significant impact on mean wage 

# Creating factor/dummy variable for race (Will divide into white and others)

elementary_middle_school_teachers <- elementary_middle_school_teachers[race == 1, race_dummy := "white"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[race != 1, race_dummy := "other"]

# Looking at the result
datasummary(wph*factor(race_dummy)  ~ Mean + Percent()+ SD + Min + Max + P25 + P75 + N , data = elementary_middle_school_teachers)
#same is the case for race

# Creating factor variable for sex as gender

elementary_middle_school_teachers[sex == 1, gender := "male"]
elementary_middle_school_teachers[sex == 2, gender := "female"]

# Looking at the result
datasummary(wph*factor(gender)  ~ Mean + Percent()+ SD + Min + Max + P25 + P75 + N , data = elementary_middle_school_teachers)
#There is a difference between males and females wage. With slightly higher wage per hour for males.
#Majority are females in this occupation

#One other factor that could have an impact on wages is that whether a person owns a child 
#Modifying the ownchild variable to binary, no matter the number of children, it will be 1 if there is a child
#present and 0 if there is no child 

# Counting the number of observations by Ownchild
elementary_middle_school_teachers[,.(count=.N), by= ownchild]

# Create if individuals own child or no
elementary_middle_school_teachers <- elementary_middle_school_teachers %>% mutate(ownchild=case_when(
  ownchild==0 ~ 0,
  TRUE ~ 1))
#seeing the result
datasummary( wph*factor(ownchild) ~ N + Percent() + SD + Mean, data = elementary_middle_school_teachers ) 

#no difference between the two wage per hour 

# Exploring the class varibles
datasummary( wph*factor(class) ~ N + Percent() + Mean + SD , data = elementary_middle_school_teachers)

#There does not seem to be much difference within government and private classes. So will club them 
#together 

elementary_middle_school_teachers <- elementary_middle_school_teachers[class == "Government - Federal"|
                       class=="Government - Local"|class=="Government - State", Sector := "Government"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[class == "Private, For Profit"| 
                                                      class=="Private, Nonprofit", Sector := "Private"]

datasummary( wph*factor(Sector) ~ N + Percent() + Mean, data = elementary_middle_school_teachers ) 

#same is the case for their wage per hour 

# unionmme
datasummary( wph*factor(unionmme) ~ N + Percent() + Mean, data = elementary_middle_school_teachers)

#Higher wage per hour for teachers who are in the union 
# state 
datasummary( wph*factor(state) ~ N + Percent() + Mean, data = elementary_middle_school_teachers )

# prcitshp
datasummary( wph*factor(prcitshp) ~ N + Percent() + Mean, data = elementary_middle_school_teachers ) 

#grouping foreign and native into just two separate categories into a variable called origin

elementary_middle_school_teachers <- elementary_middle_school_teachers[prcitshp=="Native, Born Abroad Of US Parent(s)"|
                                    prcitshp=="Native, Born in PR or US Outlying Area"|prcitshp=="Native, Born In US",origin := "Native"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[prcitshp=="Foreign Born, Not a US Citizen"|
                                          prcitshp=="Foreign Born, US Cit By Naturalization",origin := "Foreign "]

datasummary( wph*factor(origin) ~ N + Percent() + Mean, data = elementary_middle_school_teachers ) 




######## Checking interaction between several variables #########

## If we see differences in the averages, between different variables, we will use an interaction term for those 
# in our regressions.

#####1

datasummary( wph*factor(race_dummy)*gender ~ N + Percent() + Mean, data = elementary_middle_school_teachers ) 
# It seems like wage is different based on race_dummy and gender, especially when the race is white 

race_gender <- ggplot(elementary_middle_school_teachers, aes(x = factor(race_dummy), y = wph,
                              fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Race",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right")



######2

datasummary(wph*stfips*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )

# Even though we can see that there is a difference for the 2, to include it in our regression and not have 
# variables number too high, I will create a variable called regions and club different states into them 


elementary_middle_school_teachers <- elementary_middle_school_teachers[stfips %in% c("WA", "OR", "MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM", "HI", "AK", "CA"), region := "west"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[stfips %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH"), region := "mid-west"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[stfips %in% c("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "NC", "SC", "GA", "FL", "DC","MD","DE"), region := "south"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[stfips %in% c("PA", "NY", "VT", "NH", "ME","MA","RI","CT","NJ"), region := "north-east"]



datasummary(wph*region*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )

#interaction term will be added as there is a difference in the mean values 

unionmme_region <- ggplot(elementary_middle_school_teachers, aes(x = region , y = wph,
                                  fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Region",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))



##### 3


datasummary( wph*factor(educ)*gender ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# The wage is different based on education and gender

educ_gender <- ggplot(elementary_middle_school_teachers, aes(x = factor(educ), y = wph,
                              fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Education",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 70), breaks = seq(0,70, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))



######## 4

datasummary( wph*educ*race_dummy*gender ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on education, race_dummy and gender

educ_race <- ggplot(elementary_middle_school_teachers, aes(x = factor(educ), y = wph,
                            fill = factor(race_dummy), color=factor(race_dummy))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Education",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))



######## 5

datasummary( wph*unionmme*gender ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on being a union member and gender

union_gender <- ggplot(elementary_middle_school_teachers, aes(x = unionmme, y = wph,
                               fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Union Membership",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))




########### 6

datasummary( wph*married_status*gender ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on marriage status and gender

married_gender <- ggplot(elementary_middle_school_teachers, aes(x = married_status, y = wph,
                                 fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Married Status",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))




########### 7

datasummary(wph*Sector*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on whether they working in a private or government institution and being a union member

unionmme_Sector <- ggplot(elementary_middle_school_teachers, aes(x = Sector , y = wph,
                                 fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Sector",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))




###########  8

datasummary(wph*prcitshp*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on citizenship state and being a union member, 


# based on above interaction, creating a new dummy for born in PR or outlying US to interact it with unionmme

elementary_middle_school_teachers <- elementary_middle_school_teachers[prcitshp == "Native, Born in PR or US Outlying Area", pr_born := "yes"]
elementary_middle_school_teachers <- elementary_middle_school_teachers[prcitshp != "Native, Born in PR or US Outlying Area", pr_born := "no"]

# Checking the interaction of this new variable with unionmme
datasummary(wph*pr_born*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# There is significant difference in mean wage based on pr_born and being a union member

unionmme_prborn <- ggplot(elementary_middle_school_teachers, aes(x = pr_born , y = wph,
                                  fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Born in PR or US Outlying Area",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))



########### 9

datasummary(wph*factor(race_dummy)*unionmme  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is not very different based on race_dummy and being a union member, so no need for an interaction

unionmme_race <- ggplot(elementary_middle_school_teachers, aes(x = race_dummy , y = wph,
                                fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Race",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))



######## 10

datasummary(wph*factor(ownchild)*gender  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on presence of child

#we have already converted the variable of ownchild to a binary one.

ownchild_gender <- ggplot(elementary_middle_school_teachers, aes(x = factor(ownchild) , y = wph,
                                  fill = gender, color=gender)) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Number of Children",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right")


######### 11


datasummary(wph*race_dummy*married_status  ~ N + Percent() + Mean, data = elementary_middle_school_teachers )
# It seems like wage is different based on race_dummy and married status, so no need for an interaction for this.
# especially for people who are separated 

race_married <- ggplot(elementary_middle_school_teachers, aes(x = married_status , y = wph,
                               fill = race_dummy, color=race_dummy)) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Married Status",y = "Wage per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 40), breaks = seq(0,40, 10))+
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle=45, vjust=.5))






  
  
  
  
  