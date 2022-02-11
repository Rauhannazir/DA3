#############################
##     Data analysis 3     ##
##                         ##
##       Assignment 2.     ##
##                         ##
##     Data Preparation    ##
#############################


# CLEAR MEMORY
rm(list=ls())


#install.packages("ggcorrplot")
library(tidyverse)
library(stargazer)
library(Hmisc)
library(ggcorrplot)
library(dplyr)
library(data.table)
library(skimr)
library(modelsummary)
library(scales)
library(ggpubr)

toronto_cleaned <- readRDS(gzcon(url("https://github.com/Rauhannazir/DA3/blob/main/Assignment_2/Data/clean/toronto_cleaned.RDS?raw=true")))

source("da_helper_functions.R")
source("theme_bg.R")

str(toronto_cleaned)

# check for different property types, to filter for the property types that are apartments or similar to the apartments 
types <- toronto_cleaned %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)


rm(types)

#Below are the property types that I am deciding to keep for now 

# Entire loft
# Entire serviced apartment
# Entire home/apt
# Entire Condominium
# entire guest suite


# Filtering the dataset according to the decison made 
toronto_cleaned <-toronto_cleaned %>% filter(property_type %in% c("Entire loft","Entire serviced apartment","Entire home/apt","Entire condominium (condo)"))


# Only keeping those listings that accommodate 2-6 people as the assumption is that an apartment can accomodate people between this range  
toronto_cleaned <- toronto_cleaned[toronto_cleaned$accommodates >= 2 & toronto_cleaned$accommodates <= 6,]


# Cleaning the variables 

## Firstly creating factors for variables that will be used in the model, so that they can be used in the models efficiently

# Property type
toronto_cleaned %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#I will change the values in the property type to make them cleaner and remove the redundant words 

toronto_cleaned$property_type[toronto_cleaned$property_type == "Entire condominium (condo)"] <- "Condo"
toronto_cleaned$property_type[toronto_cleaned$property_type == "Entire loft"] <- "Loft"
toronto_cleaned$property_type[toronto_cleaned$property_type == "Entire serviced apartment"] <- "Apartment"
toronto_cleaned$property_type[toronto_cleaned$property_type == "Entire home/apt"] <- "Apartment"

#Creating a new variable to store their values as factors 
toronto_cleaned <- toronto_cleaned %>% 
  mutate( f_property_type = factor(property_type))

str(toronto_cleaned$f_property_type)


# Checking for all the unique values in the room type 
unique(toronto_cleaned$room_type)

#There is only one type of value for all listings, so there is no point in keeping this variable, as it won't help in the prediction
# due to no variance 
toronto_cleaned$room_type <- NULL

# neighbourhood_cleansed as factors
#I am going to export the unique values list so that I dont have to type all the values in the as factor command, as a txt file
# would be more manageable to get the values in the right syntax and then copy paste from there 
capture.output(unique(toronto_cleaned$neighbourhood_cleansed), file = "neighbourhoodlist.txt")


toronto_cleaned <- toronto_cleaned %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c(    "Waterfront Communities-The Island" ,  "Bay Street Corridor"                 ,"Church-Yonge Corridor",              
                                                                                  "Niagara"                            , "Woburn"                              ,"South Parkdale"        ,             
                                                                                  "Junction Area"                       ,"Oakridge"                            ,"Cabbagetown-South St.James Town",    
                                                                                  "Danforth"                            ,"Willowdale East"                     ,"Oakwood Village",                    
                                                                                  "Roncesvalles"                        ,"High Park North"                     ,"Leaside-Bennington",                 
                                                                                  "Annex"                               ,"Forest Hill South"                   ,"Kensington-Chinatown",               
                                                                                  "Regent Park"                         ,"Little Portugal"                     ,"Dovercourt-Wallace Emerson-Junction",
                                                                                  "South Riverdale"                     ,"Moss Park"                           ,"Pleasant View",                      
                                                                                  "Agincourt South-Malvern West"        ,"Blake-Jones"                         ,"North St.James Town",                
                                                                                  "Islington-City Centre West"          ,"Cliffcrest"                          ,"Weston-Pellam Park",                 
                                                                                  "Trinity-Bellwoods"                   ,"Banbury-Don Mills"                   ,"Yonge-St.Clair",                     
                                                                                  "Palmerston-Little Italy"             ,"Greenwood-Coxwell"                   ,"Bendale",                            
                                                                                  "Woodbine-Lumsden"                    ,"Rosedale-Moore Park"                 ,"Parkwoods-Donalda",                  
                                                                                  "Mount Pleasant East"                 ,"Mount Pleasant West"                 ,"Clairlea-Birchmount",                
                                                                                  "Mimico (includes Humber Bay Shores)" ,"Newtonbrook West"                    ,"University",                         
                                                                                  "Tam O'Shanter-Sullivan"              ,"Playter Estates-Danforth"            ,"High Park-Swansea",                  
                                                                                  "Etobicoke West Mall"                 ,"North Riverdale"                     ,"Bayview Village",                    
                                                                                  "Casa Loma"                           ,"Scarborough Village"                 ,"The Beaches",                        
                                                                                  "Black Creek"                         ,"Yonge-Eglinton"                      ,"Willowdale West",                    
                                                                                  "Rockcliffe-Smythe"                   ,"Woodbine Corridor"                   ,"Thistletown-Beaumond Heights",       
                                                                                  "Dufferin Grove"                      ,"Wychwood"                            ,"Don Valley Village",                 
                                                                                  "Long Branch"                         ,"Rexdale-Kipling"                     ,"East End-Danforth" ,                 
                                                                                  "O'Connor-Parkview"                   ,"Keelesdale-Eglinton West"            ,"L'Amoreaux"         ,                
                                                                                  "Yorkdale-Glen Park"                  ,"Old East York"                       ,"Corso Italia-Davenport",             
                                                                                  "Downsview-Roding-CFB"                 ,"Briar Hill-Belgravia"                ,"Broadview North" ,                   
                                                                                  "Victoria Village"                    ,"Birchcliffe-Cliffside"               ,"Thorncliffe Park" ,                  
                                                                                  "Eringate-Centennial-West Deane"      ,"York University Heights"             ,"Danforth East York",                 
                                                                                  "Stonegate-Queensway"                 ,"Clanton Park"                        ,"Alderwood",                          
                                                                                  "Lansing-Westgate"                     ,"Lawrence Park South"                 ,"Henry Farm" ,                        
                                                                                  "Newtonbrook East"                     ,"Humber Heights-Westmount"            ,"Kennedy Park",                       
                                                                                  "Lawrence Park North"                 ,"Bayview Woods-Steeles"               ,"Humewood-Cedarvale",                 
                                                                                  "West Humber-Clairville"              ,"Bathurst Manor"                      ,"Kingsview Village-The Westway",      
                                                                                  "Lambton Baby Point"                  ,"Runnymede-Bloor West Village"        ,"Glenfield-Jane Heights" ,            
                                                                                  "Pelmo Park-Humberlea"                  ,"Maple Leaf"                          ,"Weston"               ,              
                                                                                  "Steeles"                             ,"Caledonia-Fairbank"                   ,"Forest Hill North"     ,             
                                                                                  "Edenbridge-Humber Valley"            ,"St.Andrew-Windfields"                ,"Kingsway South"       ,              
                                                                                  "Milliken"                            ,"Ionview"                             ,"New Toronto"           ,             
                                                                                  "Dorset Park"                         ,"Westminster-Branson"                 ,"Beord Park-Nortown"   ,            
                                                                                  "Wexford/Maryvale"                    ,"Beechborough-Greenbrook"             ,"Flemingdon Park"         ,           
                                                                                  "Hillcrest Village"                   ,"Morningside"                         ,"Willowridge-Martingrove-Richview",   
                                                                                  "Brookhaven-Amesbury"                 ,"Eglinton East"                       ,"Agincourt North"                 ,   
                                                                                  "Malvern"                             ,"Bridle Path-Sunnybrook-York Mills"   ,"Mount Olive-Silverstone-Jamestown",  
                                                                                  "Englemount-Lawrence"                 ,"Elms-Old Rexdale"                    ,"Highland Creek"                     ,
                                                                                  "West Hill"                           ,"Princess-Rosethorn"                  ,"Rouge"                              ,
                                                                                  "Guildwood"                           ,"Humber Summit"                       ,"Humbermede"                         ,
                                                                                  "Taylor-Massey"                      
  )))  

str(toronto_cleaned$f_neighbourhood_cleansed)

# get host_response_time as factors
unique(toronto_cleaned$host_response_time)
#There are some NA values. Counting them again to see what needs to be done 
toronto_cleaned[, .(Count = .N), by = host_response_time][order(Count,decreasing=TRUE)]
#Still less than 30% NAs, so it can still be useful. So will make a factor variable for this as well

toronto_cleaned <- toronto_cleaned %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))
toronto_cleaned %>% 
  group_by(f_host_response_time) %>% 
  summarise(cnt = n())

str(toronto_cleaned$f_host_response_time)

#### Handling NUMERIC VARIABLES ####

## Creating Numerical variables

#Creating numeric variables for host response and acceptance rates 
toronto_cleaned <- toronto_cleaned %>%
  mutate( p_host_response_rate = as.numeric(host_response_rate),
          p_host_acceptance_rate = as.numeric(host_acceptance_rate))

#Will also see the number of NAs in each variable 
#NA 1321
toronto_cleaned %>% 
  group_by(p_host_acceptance_rate) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#NA 1445
toronto_cleaned %>% 
  group_by(p_host_response_rate) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#NAs less than 40% 

### Clean number of bathrooms

toronto_cleaned <- toronto_cleaned %>% rename(bathrooms = bathrooms_text)
# get the number of baths from bathroom_text
toronto_cleaned$bathrooms <- as.numeric(gsub("[^0-9.-]", "", gsub("half", 0.5, toronto_cleaned$bathrooms, ignore.case = T)))
unique(toronto_cleaned$bathrooms)

toronto_cleaned %>% 
  group_by(bathrooms) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

# dropping air-bnbs where there are no washrooms 
toronto_cleaned <- subset(toronto_cleaned, bathrooms!=0)

# I am also converting the decimal number of washrooms to whole ones by rounding up, as I am considering half washrooms as 
# a single washroom as well. This way the results will be more interpretable and make more sense 

toronto_cleaned$bathrooms[toronto_cleaned$bathrooms == 1.5] <- 2
toronto_cleaned$bathrooms[toronto_cleaned$bathrooms == 2.5] <- 3
toronto_cleaned$bathrooms[toronto_cleaned$bathrooms == 3.5] <- 4

#There is only a single listing with 4 washrooms, so I am just going to drop that as well
toronto_cleaned <- subset(toronto_cleaned, bathrooms!=4)

str(toronto_cleaned$bathrooms)

################################################
# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")
toronto_cleaned <-  toronto_cleaned %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- toronto_cleaned %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(toronto_cleaned))
colnames(toronto_cleaned)[nnames_i] <- paste0("n_", numericals)


######create days since first review######3
toronto_cleaned <- toronto_cleaned %>%
  mutate(
    n_days_sincefirst = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                     as.Date(first_review ,format="%Y-%m-%d")))


#NA 931
toronto_cleaned %>% 
  group_by(n_days_sincefirst) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#######create days since last review##########
toronto_cleaned <- toronto_cleaned %>%
  mutate(
    n_days_sincelast = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                    as.Date(last_review ,format="%Y-%m-%d")))
#NA 931
toronto_cleaned %>% 
  group_by(n_days_sincelast) %>% 
  summarise(cnt = n()) %>% 
  arrange(- cnt)



#### DUMMY VARIABLES#####


# create dummy var
dummies <- c(names(toronto_cleaned)[seq(45,136)],"host_is_superhost", "host_identity_verified" ) 
toronto_cleaned <- toronto_cleaned %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- toronto_cleaned %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(toronto_cleaned))
colnames(toronto_cleaned)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))


## KEeping only the essential variables that might be included further in the models 
# keep columns if contain d_, n_, f_, p_, usd_ and some others
toronto_cleaned <- toronto_cleaned %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,property_type)


a <- toronto_cleaned %>% 
  group_by(price) %>% 
  summarise(cnt = n()) %>% 
  arrange( cnt)
rm(a)

# Dropping NAs if they exist in price just to make sure 
toronto_cleaned <- toronto_cleaned %>%
  drop_na(price)



### Looking at data in depth and cleaning values ###

##There are no missing values now in the neighbouhood_cleansed
toronto_cleaned %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

describe(toronto_cleaned$f_neighbourhood_cleansed)




#####################
### look at price ###
#####################
toronto_cleaned$price <- as.numeric(toronto_cleaned$price)

summary(toronto_cleaned$price)
describe(toronto_cleaned$price)

ggplot(toronto_cleaned) +
  geom_histogram(aes(price))

# Majority of the prices fall below $300 (95th percentile), while the mean is $155.2. Prices above $350 are extremely scarce and
# distributed far apart. I am taking a concious decision of dropping the listings which are valued at greater than 350, as I
#believe that they do not fit the trend and will only compromise the accuracy of the predictive models that we are going to make



toronto_cleaned %>% filter(price> 350) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

toronto_cleaned <- toronto_cleaned %>% filter(price< 350)

toronto_cleaned$price<- as.numeric(as.character(toronto_cleaned$price))

# Checking the price distributions of absolute and log prices, to see which one resembles the normal distribution more 
# and hence be included in the models

price_hist <- ggplot(toronto_cleaned, aes( x = price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "red", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("Price (Dollars)")
price_hist



ln_price_hist <- ggplot(toronto_cleaned, aes(x = log(price))) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "red", color = "black") +
  theme_bw() +
  scale_y_continuous(labels = label_percent()) +
  ylab("Percent") + 
  xlab("ln(Price, Dollars)")
ln_price_hist

#The absolute price values are more normally distributed, hence there is no need to take log, as it makes the distribution a
# long left tail rather than normal

#This can be seen more clearly by putting them together side by side 
price_hist_grid <- ggarrange(
  price_hist,
  ln_price_hist,
  nrow = 1) 

#Adding the note about the data filtering we have done before this
annotate_figure(price_hist_grid,bottom = 
                  text_grob("Note: Apartments with 2-6 accommodation capacity. Histogram with price < 350 Dollars"))




### Handling missing values ####

#Checking again which variables and how many exactly missing values do they have 

to_filter <- sapply(toronto_cleaned, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#f_host_response_time      p_host_response_rate   p_host_acceptance_rate      n_bedrooms                 n_beds 
#1348                      1348                   1219                        374                        71 
#n_review_scores_rating    n_reviews_per_month    n_days_sincefirst           n_days_sincelast           f_neighbouhood_cleansed
#871                       871                    871                         871                        3





# Impute the columns 
toronto_cleaned <-  toronto_cleaned %>%
  mutate(
    flag_f_neighbourhood_cleansed=ifelse(is.na(f_neighbourhood_cleansed),1,0),
    
    flag_days_sincelast=ifelse(is.na(n_days_sincelast),1, 0),
    n_days_sincelast =  ifelse(is.na(n_days_sincelast), median(n_days_sincelast, na.rm = T), n_days_sincelast),
    
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    
    n_beds = ifelse(is.na(n_beds), round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
    n_beds = ifelse(n_beds == 0, round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
    
    n_bedrooms = ifelse(is.na(n_bedrooms), n_accommodates %% 2, n_bedrooms),#assume that bedrooms correlate to around half the number of accommoda
    
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms) #assume at least 1 bath
    
  )


### adding term missing value for neighbourhood
toronto_cleaned$f_neighbourhood_cleansed <- as.character(toronto_cleaned$f_neighbourhood_cleansed)
toronto_cleaned$f_neighbourhood_cleansed[is.na(toronto_cleaned$f_neighbourhood_cleansed)] <- "Missing Value"
toronto_cleaned$f_neighbourhood_cleansed <- factor(toronto_cleaned$f_neighbourhood_cleansed)


## Dropping the below columns as they are not important for the predictive models and on top of that have a lot of NAs

toronto_cleaned$f_host_response_time <- NULL
toronto_cleaned$p_host_acceptance_rate <- NULL
toronto_cleaned$p_host_response_rate <- NULL
toronto_cleaned$n_days_sincefirst <- NULL

################################################
# look at some key variable &  functional form #
################################################


## n_accomodates:

toronto_cleaned%>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

price_vs_accommodates <- ggplot(data = toronto_cleaned, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,310)+
  xlim(0,7)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[2], se=FALSE)+
  theme_bg()
price_vs_accommodates



############################
## n_bathrooms
############################


ggplot(toronto_cleaned, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "white", alpha = 0.8) +
  xlab("Number of bathrooms") +
  theme_bw()

toronto_cleaned %>%
  group_by(n_bathrooms) %>%
  summarise(mean_price = mean(price), n = n())

#We can see that the average price is increasing with the number of bathrooms increasing, so it can be an important variable in 
# the prediction models 

# check number of bathrooms for different number of accommodates
toronto_cleaned %>% 
  group_by(n_accommodates) %>% 
  summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))

# Converting it to a factor and making sure we have washrooms equals to 1,2 or 3
toronto_cleaned <-  toronto_cleaned %>%
  mutate(f_bathrooms = cut(n_bathrooms, c(0,2,3,4), labels=c(1,2,3), right = F) )
unique(toronto_cleaned$f_bathroom)

toronto_cleaned %>% 
  group_by(f_bathrooms) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

############################
#### n_beds
###########################

ggplot(toronto_cleaned, aes(n_beds)) +
  geom_histogram( fill = "red", color = "white", alpha = 0.8, size = 0.25) +
  xlab("No of beds") +
  theme_bw()

toronto_cleaned %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of beds for different number of accommodates
toronto_cleaned %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))


#Pool accommodations with 1,2,3 (pooling 2,3 and 4 as 2) and (more than 4 as 3) 
#the rationale behind this was that there there is no difference between the mean prices 
toronto_cleaned <- toronto_cleaned %>%
  mutate(f_beds = cut(n_beds, c(0,2,5,9), labels=c(1,2,3), right = F) )

toronto_cleaned %>% 
  group_by(f_beds) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

############################
## n_bedrooms
############################

ggplot(toronto_cleaned, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "white", alpha = 0.8, size = 0.25) +
  xlab("Number of bedrooms") +
  theme_bw()

toronto_cleaned %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of bedrooms for different number of accommodates
toronto_cleaned %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_beds, na.rm = T))

describe(toronto_cleaned$n_bedrooms)

#Pool accomomdations with 1,2,3,9 beds
toronto_cleaned <- toronto_cleaned %>%
  mutate(f_bedrooms = cut(n_bedrooms, c(0,1,4,6), labels=c(1,2,3), right = F) )


toronto_cleaned %>% 
  group_by(f_bedrooms) %>% 
  summarise(cnt=n())


############################
## n_review_scores_rating
ggplot(data = toronto_cleaned, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[2], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bw()

describe(toronto_cleaned$n_review_scores_rating)

toronto_cleaned %>% 
  group_by(n_review_scores_rating) %>% 
  summarise(cnt=n()) %>% 
  arrange(-cnt)


# (0-1/2-4/4-5.5)
toronto_cleaned <- toronto_cleaned %>%
  mutate(f_review_scores_rating = cut(n_review_scores_rating, c(0,1,4,6), labels=c(1,2,3), right = F) )

toronto_cleaned %>% 
  group_by(f_review_scores_rating) %>% 
  summarise(cnt=n()) %>% 
  arrange(-cnt)


############################
## n_number_of_reviews
toronto_cleaned %>%
  filter(n_number_of_reviews <300) %>% 
  ggplot(aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "red", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bw()

toronto_cleaned %>%
  filter(n_number_of_reviews <250) %>% ggplot(aes(x=log(n_number_of_reviews) , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[2], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bw()


toronto_cleaned %>%
  filter(n_number_of_reviews <250) %>% ggplot(aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour="red", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[2], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bw()



describe(toronto_cleaned$n_number_of_reviews)
toronto_cleaned %>% 
  group_by(n_number_of_reviews) %>% 
  summarise(cnt=n())

##pool(0-1/1-99/99-max)
toronto_cleaned <- toronto_cleaned %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,99,max(toronto_cleaned$n_number_of_reviews)+1), labels=c(1,2,3), right = F))

toronto_cleaned %>% 
  group_by(f_number_of_reviews) %>% 
  summarise(cnt=n())

############################
## n_minimum_nights


toronto_cleaned %>% 
  group_by(n_minimum_nights) %>% 
  summarise(num_values=n()) %>% 
  arrange(num_values)



toronto_cleaned %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

ggplot(toronto_cleaned, aes(n_minimum_nights)) +
  geom_histogram( fill = "red", color = "white", alpha = 0.8, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_bw()

#minimum nights vary mostly between a couple of nights or almost for the whole month

describe(toronto_cleaned$n_minimum_nights)

# Pool and categorize the number of minimum nights: 1,4,3, 3+
toronto_cleaned <- toronto_cleaned %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,4,6,max(toronto_cleaned$n_minimum_nights)+1), labels=c(1,2,3), right = F))

toronto_cleaned %>% 
  group_by(f_minimum_nights) %>% 
  summarise(cnt = n())


############################
## n_n_number_of_reviews
skimr::skim(toronto_cleaned$n_number_of_reviews)

ggplot(data = toronto_cleaned, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour="red", shape=4) +
  geom_smooth(method="loess", colour=color[2], se=F)+
  labs(x="number of days since first review",y=" daily price")+
  theme_bw()


saveRDS(toronto_cleaned, "toronto_final.RDS")















