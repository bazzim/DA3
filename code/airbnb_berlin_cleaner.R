# Cleaning London airbnb file
# v.1.2. 2021-01-04 paths changed


# IN data from web
# out: airbnb_london_cleaned.csv

#setting working directory
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
# setwd("")
setwd("/Users/mariam/Desktop/DA_3/Assignments/airbnb_berlin")
dir <- "data"

#location folders
data_in  <- paste0(dir,"/raw/")
data_out <- paste0(dir,"/clean/")

library(tidyverse)
library(skimr)

# zero step
# not necessary
data <- read.csv(paste0(data_in,"airbnb_berlin_listing.csv"))

drops <- c("last_scraped","host_thumbnail_url","host_picture_url","picture_url","listing_url",
           "thumbnail_url","host_url","description","neighborhood_overview","host_location",
           "host_about","host_response_time","calendar_updated","bathrooms","availability_60",
           "availability_30", "availability_90", "availability_365", "number_of_reviews_l30d")
data <- data[ , !(names(data) %in% drops)]

#####################################
# copy data
df <- data
              
#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variable
df$price = as.numeric(gsub("[\\$,]", "", df$price))

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","has_availability","host_identity_verified","license","instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

# change host_since from character to date
df$host_since <- as.Date(df$host_since)
df$first_review <- as.Date(df$first_review)
df$last_review <- as.Date(df$last_review)

#rename variables
names(df)[names(df)=="bathrooms_text"] <- "bathrooms"

# replace half with 0.5
df$bathrooms <- ifelse(df$bathrooms %in% c("Half-bath","Shared half-bath","Private half-bath"), 0.5,df$bathrooms)
df$bathrooms <- parse_number(df$bathrooms, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE)

# replace with NA
df$host_response_rate<- replace(df$host_response_rate, df$host_response_rate=="N/A",NA)
df$host_acceptance_rate<- replace(df$host_acceptance_rate, df$host_acceptance_rate=="N/A",NA)
df$license <- replace(df$license, df$license=="N/A",NA)

# change data types to numeric
df$host_response_rate <- as.numeric(df$host_response_rate)
df$host_acceptance_rate <- as.numeric(df$host_acceptance_rate)


# #amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

# #define levels and dummies 
levs <- unique(levels(factor(unlist(df$amenities))))

# df with amenities
df <- cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

#sum(duplicated(colnames(df)))
df <- df[, !duplicated(colnames(df))]

# #host verifications
# df$host_verifications<-gsub("\\[","",df$host_verifications)
# df$host_verifications<-gsub("\\]","",df$host_verifications)
# df$host_verifications<-gsub('\\"',"",df$host_verifications)
# df$host_verifications<-as.list(strsplit(df$host_verifications, ","))


# # #define levels and dummies 
# levs_2 <- unique(levels(factor(unlist(df$host_verifications))))
# 
# # df with amenities
# df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$host_verifications, factor, levs_2), table))))

# Trim whitespaces from column names
colnames(df) <- gsub("^ *",'',colnames(df))

for (col in 1:ncol(df)){
  # clean amenities column names
  colnames(df)[col] <-  gsub(".", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("  ", " ", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u2013", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u00a0", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u2019", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u2014", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u20ac", "", colnames(df)[col], fixed=TRUE) 
  colnames(df)[col] <-  gsub("\\u00b4", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u00f", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u00e4", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\u00", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("/", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub("\\", "", colnames(df)[col], fixed=TRUE)
}


for (col in 1:ncol(df)){
  # clean column names
  colnames(df)[col] <-  gsub("'", "", colnames(df)[col], fixed=TRUE)
  colnames(df)[col] <-  gsub(" ", "_", colnames(df)[col], fixed=TRUE)
}

# delete duplicate columns
df <- df[, !duplicated(colnames(df))]

df <- df %>% select(-contains("soap"))
df <- df %>% select(-contains("conditioner"))
df <- df %>% select(-contains("breakfast"))
df <- df %>% select(-contains("Self-parking"))
df <- df %>% select(-contains("shampoo"))
df <- df %>% select(-contains("maker"))
df <- df %>% select(-contains("Paid_parking"))


drops <- c("amenities", "host_verifications", "aber_man_kann_Sie_mit_nach_draudfen_nehmen_Der_Akku_hlt_ca_4h_Audferdem_befindet_sich_ein_Wlan-fhiges_Radio_in_der_Kcche",
           "hier_lsst_sich_auch_problemlos_spotifiy_connecten_sound_system", "None", "Es_ist_eine_Box_von_Samson_im_Wohn-Schlafzimmer_verfcgbar_Diese_luft_cber_ein_Aux-Kabel_Es_ist_eine_sehr_sehr_gute_Akku_Box_Der_Akku_ist_leider_nicht_mehr_der_Beste",
           "Ski-inSki-out", "LOre9al_Paris_Elvital_conditioner", "Loreal", "Garnier", "`Es_ist_eine_Box_von_Samson_im_Wohn-Schlafzimmer_verfcgbar_Diese_luft_cber_ein_Aux-Kabel_Es_ist_eine_sehr_sehr_gute_Akku_Box_Der_Akku_ist_leider_nicht_mehr_der_Beste`",
           "Dove","Dior","`Bowers_&_Wilkins`", "Nivea", "Netflix","EV_charger", "Ethernet_connection","Chromecast","Amazon_Prime_Video","Outdoor_dining_area",
           "Outlet_covers","Long_term_stays_allowed","Luggage_dropoff_allowed","Bowers_&_Wilkins","Laundromat_nearby","Spa","Single_level_home","Paid_street_parking_off_premises", 
           "Paid_street_parking_off_premises__EUR10_per_day", "Outdoor_furniture", "Onsite_restaurant__Pentalounge", "Onsite_bar__Pentalounge", "Onsite_bar__Sky_Sports_Bar", "Onsite_bar",
           "High_chair", "Freezer", "Fireplace_guards", "Fire_pit", "Record_player", "Restaurant", "Room-darkening_shades", "Sauna", "Slippers", "premium_cable", "Piano", "Mosquito_net",
           "Private_living_room", "House_bikes", "Concierge", "Turndown_service", "standard_cable", "Shower_gel", "Lake_access", "Portable_fans", "Drying_rack_for_clothing", "Complimentary_self_parking",
           "Ceiling_fan", "Building_staff","BBQ_grill", "Bluetooth_speaker", "Onsite_bar_Sky_Sports_Bar", "Trash_compactor", "Onsite_restaurant_Pentalounge", "Minibar", "Bottled_water",
           "Barbecue_utensils", "Laundry_services", "Onsite_bar_Pentalounge", "Paid_street_parking_off_premises_EUR10_per_day", "Private_entrance", "Window_guards",
           "Cleaning_products", "Beach_essentials", "Room_service")
df <- df[ , !(names(df) %in% drops)]


## Aggregate common amenities columns  
  
## WIFI category agg
  # Subset columns which contains wifi
  new_df <- df %>% select(contains("Wifi"),"id")
  # exclude column that does not belong
  new_df <- new_df[-1]
  
  #Check if any of the rows has a 1. If it does, populate new column with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save to another dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by id
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and id column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("wifi" = "col_name"))
  

  ## TV category agg
  # Subset columns which contains TV
  new_df <- df %>% select(contains("TV"),"id")
  
  #Check if any of the rows has a 1. If it does, populate new column with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save to another dataframe. We will merge this df to the original df
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by id
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and id column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("TV" = "col_name"))
  
  

  ## Air Conditioning category agg
  # Subset columns which contains a specific word.
  new_df <- df %>% select(contains("air_")|contains("AC_"),"id")
  
  new_df <- new_df[-3]
  
  #Check if any of the rows has a 1. If it does, populate new column with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save to another dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by id
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and id column from the new_df
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("air_conditioning" = "col_name"))
  
  
  
  ## Washer category agg
  # Subset columns which contains a specific word.
  new_df <- df %>% select(contains("Washer"),"id")
  
  # Remove dishwasher
  new_df <- new_df[-1]
  
  #Check if any of the rows has a 1. If it does, populate new column with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save to another dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by id
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("washer" = "col_name")) 
  
  
  
## Sound category agg
  # Subset columns which contains sound
  new_df <- df %>% select(contains("sound"),"id")
  
  #Check if any of the rows has a 1. If it does, populate new column with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save to another dataframe. We will merge this df to the original df
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by id
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and id column from new_df
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("sound_system" = "col_name"))


## Stove or Oven category agg
# Subset columns which contains oven
new_df <- df %>% select(contains("oven")|contains("stove"),"id")

#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename aggregated column
  df <- rename(df,c("stove_or_oven" = "col_name"))  
  
  
  
## Dryer category agg
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("Dryer"),"id")
  
# Remove hair dryer
new_df <- new_df[-4]

#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)
  
 #merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("dryer" = "col_name")) 
  
  
## Workdspace category agg
 # Subset columns which contains a specific word.
new_df <- df %>% select(contains("work"),"id")
  
#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("workspace" = "col_name"))
  
  
## Refrigerator category agg
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("refrigerator")|contains("fridge"),"id")
  
#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df 
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("refrigerator" = "col_name")) 
    
  
  
## Free_parking category agg
# Subset columns
new_df <- df %>% select(contains("Free_"),"id")
  
#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)

  
#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("free_parking" = "col_name"))

  
  
## Kid-frienndly category agg
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("baby")|contains("Changing_")|contains("children")|contains("crib"), "Table_corner_guards","id")
  
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("kid_friendly" = "col_name")) 
  

  
## Heating category agg
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("heat"),"id")
  
#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe
new_df_merge <- new_df %>% select(id,col_name)

#merge original dataframe and new_df_merge by id
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("heating" = "col_name")) 
  

  
## bedroom essentials category agg
# Subset of columns to agg
new_df <- df %>% select(contains("bed"), "linens", "Extra_pillows_and_blankets", "id")

# remove bedrooms and bed
new_df <- new_df[-1]
new_df <- new_df[-1]
  
#Check if any of the rows has a 1. If it does, populate new column with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and id column from the new_df
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("bedroom_essentials" = "col_name")) 
  

  
## kitchen ware category agg
# Subset columns which contains a specific word.
new_df <- df %>% select("Baking_sheet","Cooking_basics", "Dishes_and_silverware", "Hot_water_kettle",
                          "Keurig_coffee_machine", "Nespresso_machine", "Microwave", "Toaster", "Pour-over_coffee",
                          "Wine_glasses","id")

#Ceck if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("kitchenware" = "col_name")) 
  
  

## entertainment category agg
# Subset columns which contains a specific word
new_df <- df %>% select(contains("game"),"id")
  
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("entertainment" = "col_name"))   
  
  
  
## Clothing_storage category agg

new_df <- df %>% select(contains("cloth"), "id")
#new_df <- new_df[-4]
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("clothing_storage" = "col_name"))   
  
  
  
## toiletries category agg
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("toil"), "id")
  
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("toiletries" = "col_name"))
  
 
## gym or fitness centre category
# Subset columns which contains a specific word.
new_df <- df %>% select("Gym","24-hour_fitness_center", "id")
  
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))

# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)

#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)

#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))

# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))

# Rename aggregated column
df <- rename(df,c("fitness_centre" = "col_name"))
  
  
  
## garden, balcony, patio category
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("garden")|contains("backyard")| contains("beachf")|contains("balcony")|contains("lake"),"Waterfront", "id")
  
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("garden_backyard_balcony_agg" = "col_name"))  
  
  
  
## Kitchen category
# Subset columns which contains a specific word.
new_df <- df %>% select(contains("kitchen"), "id")
new_df <- new_df[-3]
#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)
  
#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))
  
# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))
  
# Rename aggregated column
df <- rename(df,c("kitchen" = "col_name"))

## Safe box category
# Subset columns which contains a specific word.
new_df <- df %>% select("Lockbox","Safe", "id")

#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))

# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)

#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)

#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))

# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))

# Rename aggregated column
df <- rename(df,c("safe_box" = "col_name"))



## essentials category
new_df <- df %>% select("Bathroom_essentials","Essentials", "id")

#Check if any of the rows has a 1. If it does, populate new column 'col_name' with 1
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))

# Save to another dataframe. We will merge this df to the original df
new_df_merge <- new_df %>% select(id,col_name)

#merge original dataframe and new_df_merge by 'id'
df <- merge(df,new_df_merge,by = "id", all = FALSE)

#remove the new column and 'id' column from the new_df dataframe
new_df <- new_df %>% select(-c(id,col_name))

# Remove the selected columns from original dataframe since they have already been aggregated into a new column and merged
df <- df %>% select(-colnames(new_df))

# Rename aggregated column
df <- rename(df,c("essentials" = "col_name"))

data <- df
df <- data

to_convert <- colnames(df[55:101])

for(i in to_convert){
  class(df[, i]) = "integer"
}

  
glimpse(df)
skim(df)
  
# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0] 
  

#write csv
write.csv(df,file=paste0(data_out,"airbnb_berlin_cleaned.csv"))


