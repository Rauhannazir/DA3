#############################
##     Data analysis 3     ##
##                         ##
##       Assignment 2.     ##
##                         ##
##       Data cleaning     ##
#############################


##I have selected Toronto as the city I am going to do my analysis on 

#Loading the libraries and the data

rm(list=ls())
library(tidyverse)
library(dplyr)
library(data.table)
library(skimr)


#Loading the data from a rds file 
toronto_raw <- readRDS(gzcon(url("https://github.com/Rauhannazir/DA3/blob/main/Assignment_2/Data/Raw/toronto_raw.RDS?raw=true")))

#### basic data checks

#Checking the dates on which the data was scraped 
sort(unique(toronto_raw$last_scraped) )# the data was scraped on 3 dates (6th, 7th and 9th Jan 2022)

#Counting the listings that were scraped on each of the date 
toronto_raw %>% 
  group_by(last_scraped) %>% 
  summarise(uniqueid = n_distinct(id))

#There was only one listing that was scraped on the 9th of January, so I am just going to drop it 
toronto_cleaned <- subset(toronto_raw, toronto_raw$last_scraped != "2022-01-09")

sum(rowSums(is.na(toronto_cleaned)) == ncol(toronto_cleaned)) # no only NA rows
nrow(toronto_cleaned[duplicated(toronto_cleaned),]) #  no duplicates found 

#Checking how many columns have more than 50% NAs, as more than 50% is quite a lot, to check which variables need more attention,
#and drop them if they are not important for our prediction 
sum(colSums(is.na(toronto_cleaned)) > 0.5*nrow(toronto_cleaned)) # there are 3 columns with at least 50% of the values missing 

# where do we have missing variables now?
to_filter <- sapply(toronto_cleaned, function(x) sum(is.na(x)))
to_filter[to_filter > 0] #Just to check which variables have NA values in them 
#Not a lot of variables have NA values, which is quite appealing, and as found earlier there are only 3 columns with more than 50% 
#NAs, and those columns are completely null. So will just drop them right away

toronto_cleaned$bathrooms = NULL
toronto_cleaned$neighbourhood_group_cleansed = NULL
toronto_cleaned$calendar_updated = NULL

# We will get more clarity about what to do with the rest of the NAs after we filter the data down to
# apartments with 2-6 people, as those are the ones we will be only focusing on and predicting their price 

str(toronto_cleaned)
skim(toronto_cleaned)
summary(toronto_cleaned)


#It is a toronto_cleaned frame, however I will be converting it to a data table, to apply some commands of a data table 
toronto_cleaned <- as.data.table(toronto_cleaned)

#Counting the number of listings according to the room type, as we want to only predict apartments, we will drop the rest in 
#data prepartation 
toronto_cleaned[, .(Count = .N), by = property_type][order(Count,decreasing=TRUE)]

# Dropping unnecessary columns
toronto_cleaned <- toronto_cleaned %>% select(-c( "last_scraped",
                        "name",
                        "description",
                        "neighborhood_overview",
                        "picture_url",
                        "host_url",
                        "host_location",
                        "host_about",
                        "host_thumbnail_url",
                        "host_picture_url",
                        "host_total_listings_count", 
                        "minimum_minimum_nights", "minimum_maximum_nights", "minimum_nights_avg_ntm",
                        "maximum_minimum_nights", "maximum_maximum_nights", "maximum_nights_avg_ntm",
                        "number_of_reviews_ltm", "number_of_reviews_l30d",
                        "calculated_host_listings_count_entire_homes",
                        "calculated_host_listings_count_private_rooms",
                        "calculated_host_listings_count_shared_rooms"))


summary(toronto_cleaned)


# dropping broken lines - where id is not a character of numbers
toronto_cleaned$junk<-grepl("[[:alpha:]]", toronto_cleaned$id)
toronto_cleaned<-subset(toronto_cleaned,toronto_cleaned$junk==FALSE)
toronto_cleaned$junk <- NULL

# displaying the class and type of each variable, to see if we need to convert any variable type 
sapply(toronto_cleaned, class)
sapply(toronto_cleaned, typeof)


#### Data Cleaning 

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  toronto_cleaned[[perc]]<-as.numeric(gsub("%","",as.character(toronto_cleaned[[perc]])))
}


#remove dollar signs from price variables
for (i in 1:nrow(toronto_cleaned)){
  toronto_cleaned$price[i] <- as.numeric(gsub("\\$","",as.character(toronto_cleaned$price[i])))
}

toronto_cleaned$price <- as.numeric(toronto_cleaned$price)

#format binary variables. Converting true and false into ones and zeros. (will be used in the models easily)
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  toronto_cleaned[[binary]][toronto_cleaned[[binary]]=="f"] <- 0
  toronto_cleaned[[binary]][toronto_cleaned[[binary]]=="t"] <- 1
}


# The amenities variable is extremely messy. To be able to use it in our predictive models, we need to format it to a different
# format 


toronto_cleaned$amenities<-gsub("\\[","",toronto_cleaned$amenities)
toronto_cleaned$amenities<-gsub("\\]","",toronto_cleaned$amenities)
toronto_cleaned$amenities<-gsub('\\"',"",toronto_cleaned$amenities)
toronto_cleaned$amenities<-as.list(strsplit(toronto_cleaned$amenities, ","))

#define levels and dummies
levels(factor(unlist(toronto_cleaned$amenities)))


levs <- levels(factor(unlist(toronto_cleaned$amenities)))
toronto_cleaned<-cbind(toronto_cleaned,as.data.frame(do.call(rbind, lapply(lapply(toronto_cleaned$amenities, factor, levs), table))))


# create data frame of the amenities
ams <- toronto_cleaned %>% select(-(1:50))

# delete spaces in the beginning and end of the column names, and transfer all to lower case
names(ams) <- gsub(" ","_", tolower(trimws(names(ams))))

# look at the column names we have
levs <- sort(names(ams))


# merge all the columns with the same column name
ams <- as.data.frame(do.call(cbind, by(t(ams), INDICES= names(ams),FUN=colSums)))

# list of key words to merge together
cat <- c( "kitchen", "stove", "oven", "frige","o_machine|ee_machine|coffee", "gril",
          "free.*on_premises", "free.*street", "paid.*on_premis|valet", "paid.*off_premises|self-parking|parking",
          "wifi|internet", "netflix|tv.*netflix", "cable", "tv", "sound_system",
          "toiletries", "shampoo|conditioner", "body_soap|gel", "hair_dryer", "washer", "dryer", "iron",  
          "heating", "air_cond|fan", "balcony|terrace", "garden",
          "onsite_bar|restaurant", "breakfast",  "work|office", "spa",  "fitness|gym",  
          "children|baby|crib|high|corner|chang", "smoking", "housekeeping", "fireplace", "clothing_storage"
)



# function to merge columns with the same key word in them
for (i in cat) {
  tdf <- ams %>% select(matches(i))
  
  ams$new_col <- ifelse(rowSums(tdf)>0, 1, 0)
  
  names(ams)[names(ams) == "new_col"] <- paste0("have_", i)
  
  ams <- ams %>% select(-colnames(tdf)) 
  
} 

# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(ams), function(x){
  ratio <- sum(ams[[x]])/nrow(ams)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})


amenities <- ams[,selected]

toronto_cleaned <- toronto_cleaned %>% select((1:50))

toronto_cleaned <- cbind(toronto_cleaned, amenities)

#Dropping more unnecessary columns and unwanted items 
toronto_cleaned$amenities = NULL
toronto_cleaned$host_name = NULL
toronto_cleaned$host_verifications = NULL
toronto_cleaned$license = NULL

rm(amenities)
rm(ams)
rm(cat)
rm(levs)
rm(selected)
rm(i)
rm(perc)
rm(tdf)
rm(binary)


saveRDS(toronto_cleaned, "toronto_cleaned.RDS")

