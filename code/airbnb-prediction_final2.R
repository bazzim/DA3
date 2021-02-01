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

############################################################

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions

# Load libraries
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(ranger)
library(Hmisc)
library(kableExtra)


# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# use_case_dir <- "ch14-airbnb-reg/"
dir <- "data"
# data used
data_in <- paste(dir,"clean/", sep = "/")
data_out <- paste0(dir,"/clean/")

# data_out <- use_case_dir
output <- paste0(dir,"output/")
create_output_if_doesnt_exist(output)

options(digits = 3)

########################################
# PART I.
########################################

# !!! make sure you have run airbnb_prepare.R before

#############
# Load data #
#############

# Load cleaned and processed data
data <- read_csv(paste0(data_in, "airbnb_berlin_workfile_adj.csv")) %>%
       mutate_if(is.character, factor)

######################
# Quick look at data #
######################
glimpse(data)
skim(data)


# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


###################################
# Business logic- define our prediction problem
###################################

# Decision
# Size, we need a normal apartment, 2-6persons, below 1000 USD
data <- data %>%
  filter(n_accommodates %in% (2:6),
         price<1000)
# N=9891


# that's gonna be our sample
skimr::skim(data)

# save workfile
write.csv(data, paste0(data_out, "airbnb_berlin_work.csv"), row.names = F)


#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type` and the `bedroom`?
data %>%
  group_by(f_property_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
   group_by(f_bedroom) %>%
   dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)

# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price<400)


# Distribution of price by type below 400

# Histograms
# price
g3a <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
#  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
    scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a
save_fig("ch14-figure-3a-airbnb-price", dir, size = "small")

# lnprice
g3b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b
save_fig("ch14-figure-3b-airbnb-lnprice", dir, size = "small")


########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_bathrooms", "f_property_type", "f_bedroom", "n_beds", "d_air_conditioning")

# Factorized variables
basic_add <- "f_bathroom"
reviews <- c("n_number_of_reviews","n_review_scores_rating", "d_dishwasher")
# Higher orders
poly_lev <- c("n_accommodates2")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_minimum_nights", "d_air_conditioning", "Room type", "Air")
p2 <- price_diff_by_variables2(data, "f_minimum_nights", "d_tv", "Room type", "Property type")
p1
p2
#Look up canelation policy
p3 <- price_diff_by_variables2(data, "f_bedroom", "d_elevator", "Number of bedrooms", "Elevator")
p4 <- price_diff_by_variables2(data, "f_bedroom", "d_free_parking", "Number of bedrooms", "Free Parking")
p3
p4
#Look up property type
p5 <- price_diff_by_variables2(data, "f_property_type", "d_dishwasher", "Property type", "Dishwasher")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_kid_friendly", "Property type", "Air Conditioning")
p5
p6

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions

#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("ch14-figure-5-airbnb-interactions",output,"verylarge")



# dummies suggested by graphs
X1  <- c("f_property_type*f_number_of_reviews",  "f_property_type*d_air_conditioning")

# Additional interactions of factors and dummies
X2  <- c("d_air_conditioning*f_property_type", "d_dishwasher*f_property_type", "d_kid_friendly*f_property_type")
X3  <- c(paste0("(f_property_type + f_bathroom + f_bedroom + f_number_of_reviews) * (",
                paste(amenities, collapse=" + "),")"))

##pets_allowed, wifi, tv, kidfriendly, ac dummies, stove_or_oven

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180124)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")

  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))

  # Initialize values
  rmse_train <- c()
  rmse_test <- c()

  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared

  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)

    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)

  }

  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}



model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                 "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(42, 50), breaks = seq(42,50, 2)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
save_fig("ch14-figure-7-airbnb-model-result-levels", output, "small")

#################################
#           LASSO               #
#################################

# take model 8 (and find observations where there is no missing data)may
vars_model_7 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities)
vars_model_8 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                      data = data_work,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      trControl = train_control,
                      tuneGrid = tune_grid,
                    na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient>0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

# Note: re book
# The textbook contains a somewhat different table and graph for train and test RMSE. 
# The ordering is the same but the numbers are not. This is an error in the book, sorry. 


########################################
# PART III.
########################################

###################################################
# Diagnsotics #
###################################################
model3_level <- model_results_cv[["modellev3"]][["model_work_data"]]
model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]


# look at holdout RMSE
model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred
save_fig("ch14-figure-8a-level-vs-pred", output, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
            conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev7_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (Euros)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate
save_fig("ch14-figure-8b-ci-n-accomodate", output, "small")

##############################################
### Random Forest

Hmisc::describe(data$price)
describe(data$f_property_type)
describe(data$f_neighbourhood_cleansed)
table(data$f_number_of_reviews)
table(data$f_bathroom)
table(data$n_bedrooms)



dim(data_work) # 7913
dim(data_holdout) #1978

###############
# Define models: simpler, extended --------------------------------------------
basic_vars  <- c("n_accommodates", "f_bedroom", "n_minimum_nights") # "f_property_type""n_beds" "d_dishwasher" "d_air_conditioning" "f_property_type" "d_free_parking

# Factorized variables
basic_add <- "f_bathroom"
reviews <- c("n_number_of_reviews","n_review_scores_rating", "flag_n_number_of_reviews")
# Higher orders
poly_lev <- c("n_accommodates2")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# dummies suggested by graphs
x1  <- c("f_property_type*f_number_of_reviews")

# Additional interactions of factors and dummies
x2  <- c("d_air_conditioning*f_property_type", "d_dishwasher*f_property_type", "d_elevator*f_property_type") #kid_friendly
x3  <- c(paste0("(f_property_type + f_bathroom + f_bedroom + f_number_of_reviews) * (",
                paste(amenities, collapse=" + "),")"))

predictors_1 <- c(basic_vars,basic_add,reviews,poly_lev,x1,amenities)
predictors_2 <- c(basic_vars,basic_add,reviews,poly_lev,x1,x2,amenities)# removed amenities, added basic_add
predictors_E <- c(basic_vars,basic_add,reviews,poly_lev,x1,x2,amenities)


# Train control
# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = 7,
  .splitrule = "variance",
  .min.node.size = 5
)


# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_work,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1

  
# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = 12,
  .splitrule = "variance",
  .min.node.size = 5
)
  
set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_work,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
  
rf_model_2

# evaluate random forests -------------------------------------------------
  
results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)

summary(results)
  
# Save outputs -------------------------------------------------------
  
# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
    dplyr::select(mtry, min.node.size, RMSE) %>%
    dplyr::rename(nodes = min.node.size) %>%
    spread(key = mtry, value = RMSE)
  
kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
    add_header_above(c(" ", "vars" = 3)) %>%
    cat(.,file= paste0(output,"rf_tuning_modelB.tex"))
  
  
# Turning parameter choice 1
result_1 <- matrix(c(
    rf_model_1$finalModel$mtry,
    rf_model_2$finalModel$mtry,
    rf_model_1$finalModel$min.node.size,
    rf_model_2$finalModel$min.node.size
),
  nrow=2, ncol=2,
  dimnames = list(c("Model A", "Model B"),
                  c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
    cat(.,file= paste0(output,"rf_models_turning_choices.tex"))
  
# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                       mean(results$values$`model_2~RMSE`)
                       #mean(results$values$`model_2b~RMSE`)
  ),
  nrow=2, ncol=1,
  dimnames = list(c("Model A", "Model B"),
                  c(results$metrics[2]))
)
  
kable(x = result_2, format = "latex", digits = 3) %>%
    cat(.,file= paste0(output,"rf_models_rmse.tex"))
  
#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################
  
#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
    var.imp <- as.matrix(sapply(groups, function(g) {
      sum(importance(rf.obj)[g], na.rm = TRUE)
    }))
    colnames(var.imp) <- "MeanDecreaseGini"
    return(var.imp)
}
  
# variable importance plot
# 1) varimp plot , top 10
  
rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
    data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
    mutate(varname = gsub("f_neighbourhood_cleansed", "Neighborhood:", varname) ) %>%
    #mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp)) %>% head(20)
  
##############################
# 1) full varimp plot, top 10 only
##############################
  
# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(color=color[1], size=1) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
    ylab("Importance (Percent)") +
    xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bg() +
    theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
          axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b

save_fig("randomforest-varimp-top10",output, "small")
  
  
#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
    autoplot( ) +
    geom_point(color=color[1], size=2) +
    geom_line(color=color[1], size=1) +
    ylab("Predicted price") +
    xlab("Accommodates (persons)") +
    scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
    theme_bg()
pdp_n_acc_plot
#save_fig("rf_pdp_n_accom", output, "small")
save_fig("rf-pdp-n-accom", output, "small")
  
  
# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.
  
# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
    mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))
  
describe(data_holdout_w_prediction$n_accommodates)
  
  
######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )
  
###########
b <- data_holdout_w_prediction %>%
  # filter(f_neighbourhood_cleansed %in% c("Helmholtzplatz", "Alexanderplatz", "Brunnenstr. Süd", "Frankfurter Allee Süd FK" ,
  #                                       "Neuköllner Mitte/Zentrum", "Prenzlauer Berg Nordwest", "Reuterstraße",
  #                                       "Tempelhofer Vorstadt", "Buchholz", "Helmholtzplatz", "Neuköllner Mitte/Zentrum",
  #                                       "Westend", "Wiesbadener Straße", "Wilhelmstadt", "Zehlendorf  Südwest")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
      rmse = RMSE(predicted_price, price),
      mean_price = mean(price),
      rmse_norm = rmse / mean_price
  )
  
c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Entire apartment", "Entire loft", "Entire condominium", "Entire serviced apartment")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )
  
  
d <- data_holdout_w_prediction %>%
    dplyr::summarise(
      rmse = RMSE(predicted_price, price),
      mean_price = mean(price),
      rmse_norm = RMSE(predicted_price, price) / mean(price)
   )
  
# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")
  
line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Neighborhood", "", "", "")
  
result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
              `RMSE/price` = as.numeric(`RMSE/price`))
  
options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
    cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)
  
##########################################
  
#########################################################################################
#
# PART IV
# HORSERACE: compare Random Forest models with CART & GBM
#
#########################################################################################
  
# CART
set.seed(1234)
system.time({
    cart_model <- train(
      formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
      data = data_work,
      method = "rpart",
      tuneLength = 10,
      trControl = train_control
  )
})
  
fancyRpartPlot(cart_model$finalModel, sub = "")
  
# GBM  -------------------------------------------------------
# gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
#                            n.trees = (4:10)*50, # number of iterations, i.e. trees
#                            shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
#                            n.minobsinnode = c(10,20) # the minimum number of training set samples in a node to commence splitting
# )
#   
#   
# set.seed(1234)
# system.time({
#   gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#                       data = data_work,
#                       method = "gbm",
#                       trControl = train_control,
#                       verbose = FALSE,
#                       tuneGrid = gbm_grid) 
# })
# gbm_model
  
# and get prediction rmse and add to next summary table
  
# ---- compare these models
final_models <-
    list("LASSO (model w/ interactions)" = lasso_model,
         "CART" = cart_model,
         "Random forest (smaller model)" = rf_model_1,
         "Random forest" = rf_model_2)#,
         #"GBM (basic tuning)"  = gbm_model)
  
results <- resamples(final_models) %>% summary()
  
# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE
  
result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")
  
kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
    cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.tex"))
  
  
# evaluate preferred model on the holdout set -----------------------------
  
result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
  }) %>% unlist() %>% as.data.frame() %>%
    rename("Holdout RMSE" = ".")
  
kable(x = result_5, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
    cat(.,file= paste0(output,"horse_race_of_models_houldout_rmse.tex"))
  
############################################################
  