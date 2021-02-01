#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 14
# CH14B Predicting AirBnB apartment prices: selecting a regression model
# using the airbnb dataset
# version 0.91 2021-01-04
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


library(tidyverse)
library(stargazer)
library(Hmisc)
library(skimr)
library(modelsummary)

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
dir <- "data"
data_in <- paste(dir,"clean/", sep = "/")
data_out <- paste0(dir,"/clean/")

output <- "output/"
create_output_if_doesnt_exist(output)

options(digits = 3)

#-------------------------------------------------------
# Import data
data <- read_csv(paste(data_in,"airbnb_berlin_cleaned.csv", sep = "")) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))

#remove x1
data <- data[-1]

# keep if property type is Apartment
table(data$property_type)

data <- data %>%
        filter(property_type %in% c("Entire apartment", "Entire condominium", "Entire home/apt", "Entire loft",
                                    "Entire serviced apartment"))


# Property type as factor
data <- data %>%
  mutate(
    f_property_type = factor(property_type),
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

# keep apartments that accommodate 2-6 persons
data <- data %>% filter(data$accommodates %in% (2:6))

#Room type as factor
# table(data$room_type)
# data <- data %>%
#   mutate(f_room_type = factor(room_type))



#---------------------------------------------------------------------------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(
    euro_price_day = price)

#-------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews", "bedrooms",
                "minimum_nights","beds", "host_acceptance_rate", "host_response_rate", "reviews_per_month")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


#create days since first review
# data <- data %>%
#   mutate(
#     n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
#                                 as.Date(first_review ,format="%Y-%m-%d")))

# create dummy vars
dummies <- names(data)[seq(55,101)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dummies, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# remove duplicate column names
data <- data[, !duplicated(colnames(data))]

# keep columns if contain d_, n_,f_, p_, euro_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^euro_.*"), price, id,
         neighbourhood_cleansed,property_type)

# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_out, "airbnb_berlin_workfile.csv"))


##################################
# DESCRIBE

#--------------------------------
data <- read_csv(paste(data_out,"airbnb_berlin_workfile.csv", sep = ""))

N=nrow(data)
N
# N=9903

#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Remove extreme values + missing from prices (this case: only 12 missing values)
data <- data %>%
  filter(price < 1000)

# Histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice

ggsave(paste0(output, "R_F14_h_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

cairo_ps(filename = paste0(output, "R_F14_h_lnprice.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(R_F14_h_lnprice)
dev.off()

R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram( binwidth = 15,fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price
ggsave(paste0(output, "R_F14_h_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_F14_h_price.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_price)
dev.off()


################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

R_14_s_n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,800)+
  xlim(0,8)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

ggsave(paste0(output, "R_14_s_n_accommodates.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "R_14_s_n_accommodates.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(R_14_s_n_accommodates)
dev.off()


# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, 
         ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2,
         ln_beds = log(n_beds),
         ln_bedrooms = log(n_bedrooms),
         ln_number_of_reviews = log(n_number_of_reviews+1))

# Pool accommodations with 1,2,3,4 bedroom
data <- data %>%
  mutate(f_bedroom = cut(n_bedrooms, c(1,2,3,4), labels=c(0,1,2), right = F) )


# Pool accommodations with 0,1,2,8 bathrooms
data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,5), labels=c(0,1,2), right = F) )

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))


# Pool and categorize the number of minimum nights: 1,2,3, 3+
data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


#------------------------------------------------------------------------------------------------


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values? 
# 1. drop if no target
data <- data %>%
  drop_na(price)


# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    n_bedrooms = ifelse(is.na(n_bedrooms), median(n_bedrooms, na.rm = T), n_bedrooms), #assume at least 1 bedroom
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_bedroom=ifelse(is.na(f_bedroom),1, f_bedroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
    ln_bedrooms=ifelse(is.na(ln_bedrooms),0, ln_bedrooms),
  ) 

# 3. drop columns when many missing not imortant
to_drop <- c("n_host_acceptance_rate", "n_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    #flag_days_since=ifelse(is.na(n_days_since),1, 0),
    #n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month),
    flag_n_number_of_reviews=ifelse(n_number_of_reviews==0,1, 0)
  )
#table(data$flag_n_days_since)

# Look at data
datasummary_skim(data$id)
datasummary_skim(data, 'categorical' )
skimr::skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# N=9891

write_csv(data, paste0(data_out, "airbnb_berlin_workfile_adj.csv"))

#################

# Regression 1: ln price and num of accomodates and squares
reg1 <- lm(ln_price ~ n_accommodates + n_accommodates2, data=data)
# Regression 2: ln price and log num of accomodates
reg2 <- lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
reg3 <- lm(ln_price ~ n_accommodates, data=data)

#dir <- "airbnb_berlin"
stargazer( list(reg1, reg2, reg3), digits=3, out= "output/price_vs_accommodates.html",sep="")

## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

## Bedrooms
data %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bedrooms") +
  theme_bg()

## bathrooms
ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

## Number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews <100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Number of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)
# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)


# ## Time since
# # Create variables, measuring the time since: squared, cubic, logs
# data <- data %>%
#   mutate(
#     ln_days_since = log(n_days_since),
#     ln_days_since2 = log(n_days_since)^2,
#     ln_days_since3 = log(n_days_since)^3 ,
#     n_days_since2=n_days_since^2,
#     n_days_since3=n_days_since^3)
# 
# # Check the effect
# lndays_plot <- data %>%
#   filter(data$price<=800, ln_days_since>2)


skimr::skim(data$n_number_of_reviews)
ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(60,100)+
  xlim(0,20)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()

#-Inf values
#lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,800)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is

## minimum nights
lm(ln_price ~ n_minimum_nights,data=data)

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type")#, "f_room_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#########
############################

# Change Infinite values with NaNs
# for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

# Filter neighborhoods with more than 50 listings
data2 <- data %>% group_by(neighbourhood_cleansed) %>% filter(n() >= 50)

# Look at data
datasummary_skim(data, 'categorical' )
skimr::skim(data)


# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
write_csv(data, paste0(data_out, "airbnb_berlin_workfile_adj.csv"))

#------------------------------------------------------------------------------------------------

