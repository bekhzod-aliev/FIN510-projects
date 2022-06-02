#set working directory
getwd()
setwd("E:/STUDY/FIN510/Final Project")

options(scipen = 100)

#load required packages
library(tidyverse)
library(boot)
library(glmnet)
library(randomForest)
setwd("/Users/bekhzodmacbook/Downloads/Fin510 Project")

#read the data
hist <- read.csv("historic_property_data.csv")

#check variables in the dataset
names(hist)

#check percentage of missing values in each column
colMeans(is.na(hist))
#check the number of missing values in each column
map(hist, ~sum(is.na(.)))

#variables contains more than 50% missing values will be dropped
data <- within(hist, rm("meta_cdu", "char_apts", "char_tp_dsgn", "char_attic_fnsh", 
                "char_renovation", "char_porch", "char_tp_plan", 
                "char_gar1_cnst", "char_gar1_att", "char_gar1_area"))



# Exclude missing values in the dataset
data <- na.exclude(data)

# total number of rows 
dim(data)[1]
names(data)

# number of rows to select for the training set 
dim(data)[1]*0.6
set.seed(1)
# row index of the training set 
train.ind <- sample(c(1:dim(data)[1]),dim(data)[1]*0.6)

# training set
train.df <- data[train.ind,]

# validation set
test.df <- data[-train.ind,]


# fit a linear regression on the training set 
lm1 <- lm(sale_price ~ char_age + char_beds + char_bldg_sf + char_fbath
          + char_frpl + char_hbath + char_hd_sf + char_rooms + econ_midincome
          + econ_tax_rate + geo_fs_flood_risk_direction + geo_ohare_noise 
          + geo_fs_flood_factor + geo_black_perc + geo_his_perc + geo_asian_perc +geo_other_perc
          + meta_certified_est_bldg + meta_certified_est_land + ind_arms_length
          + as.factor(char_bsmt) + as.factor(char_bsmt_fin) + as.factor(char_ext_wall)
          + as.factor(char_gar1_size) + as.factor(char_heat) + as.factor(char_oheat) + geo_school_hs_district
          + as.factor(char_roof_cnst) + as.factor(char_use) + as.factor(char_cnst_qlty) + as.factor(ind_garage) + as.factor(char_bsmt_fin), data=train.df)
summary(lm1)

# stepwise regression for reducing predictors
lm.step.both <- step(lm1, direction = "both")
summary(lm.step.both)  

#making predictions and measuring prediction error
lm.step.pred.both <- predict(lm.step.both, test.df)
head(lm.step.pred.both)

# MSE in the test set 
mean((test.df$sale_price-lm.step.pred.both)^2)

# MSE:15571841433 Ella features
# MSE: 14748787790 Updated features

#----------------------------------------------------------------------#

# K-fold validation
set.seed(1)
glm1 <- glm(sale_price ~ char_age + char_beds + char_bldg_sf + char_fbath
            + char_frpl + char_hbath + char_hd_sf + char_rooms + econ_midincome
            + econ_tax_rate + geo_fs_flood_risk_direction + geo_ohare_noise 
            + geo_fs_flood_factor + geo_black_perc + geo_his_perc + geo_asian_perc +geo_other_perc
            + meta_certified_est_bldg + meta_certified_est_land + ind_arms_length
            + as.factor(char_bsmt) + as.factor(char_bsmt_fin) + as.factor(char_ext_wall)
            + as.factor(char_gar1_size) + as.factor(char_heat) + as.factor(char_oheat) + geo_school_hs_district
            + as.factor(char_roof_cnst) + as.factor(char_use) + as.factor(char_cnst_qlty) + as.factor(ind_garage) + as.factor(char_bsmt_fin), data= data)
summary(glm1)

# compute the 10-fold cross-validation prediction error
kfcv.err1 <- cv.glm(data,glm1,K=10)

# cross-validated MSE  
kfcv.err1$delta[1]
# MSE:16995628291

# Validation set Approach
# predicted mpg for records in the test set 
lm1.pred <- predict(lm1,test.df)

# MSE in the validation set 
mean((test.df$sale_price-lm1.pred)^2)
# MSE: 15557540826

#----------------------------------------------------------------------#

# Lasso Regression
set.seed(1)
df <- data[, c["sale_price", "char_age", "char_beds", "char_bldg_sf", "char_fbath",
               "char_frpl", "char_hbath", "char_hd_sf", "char_rooms", "econ_midincome",
               "econ_tax_rate", "geo_fs_flood_risk_direction", "geo_ohare_noise",
               "geo_fs_flood_factor", "geo_black_perc", "geo_his_perc", "geo_asian_perc", "geo_other_perc",
               "meta_certified_est_bldg", "meta_certified_est_land", "ind_arms_length",
               "char_bsmt", "char_bsmt_fin", "char_ext_wall",
               "char_gar1_size", "char_heat", "char_oheat", "geo_school_hs_district",
               "char_roof_cnst", "char_use" , "char_cnst_qlty", "ind_garage", "char_bsmt_fin"]


# convert a data frame of predictors to a matrix 
x <- model.matrix(sale_price~.,df)[,-1]
# outcome 
y <- df$sale_price


# row indexes of the training set 
train.index <- sample(c(1:dim(x)[1]), dim(x)[1]*0.5)

# row indexes of the test set 
test.index <- (-train.index)

# outcome in the test set 
y.test <- y[test.index]

# fit a lasso regression model
fit<- glmnet(x[train.index,],y[train.index],alpha=1)

# sequence of lambda values 
fit$lambda

# plot coefficients on log of lambda values
plot(fit, xvar="lambda")

#----------------------------------------------------------------
# model with a small lambda value
# return a small lambda value 
lambda.small <- fit$lambda[75]

# make predictions for records in the test set 
pred.lambda.small <- predict(fit,s=lambda.small,newx=x[test.index,])

# MSE in the test set 
mean((y.test-pred.lambda.small)^2)


# MSE:16038417332

#-----------------------------------------------------------------

# model with a large lambda
# return a large lambda value 
set.seed(1)
lambda.large <- fit$lambda[1]

# make predictions for records in the test set 
pred.lambda.large <- predict(fit,s=lambda.large,newx=x[test.index,])

# MSE in the test set 
mean((y.test-pred.lambda.large)^2)

# use cross-validation to choose lambda
cv.fit <- cv.glmnet(x[train.index,],y[train.index],alpha=1, type.measure="mse")

# cross-validated MSE for each lambda 
plot(cv.fit)

# lambda that corresponds to the lowest cross-validated MSE 
lambda.best <- cv.fit$lambda.min

# make predictions for records the test set 
pred.lambda.best <- predict(cv.fit,s=lambda.best,newx=x[test.index,])

# MSE in the test set 
mean((y.test-pred.lambda.best)^2)
# MSE:16038417332

#-----------------------------------------------------------------
#Random Forest
set.seed(1)
rf <- randomForest(sale_price ~ char_age + char_beds + char_bldg_sf + char_fbath
                   + char_frpl + char_hbath + char_hd_sf + char_rooms + econ_midincome
                   + econ_tax_rate + geo_fs_flood_risk_direction + geo_ohare_noise 
                   + geo_fs_flood_factor + geo_black_perc + geo_his_perc + geo_asian_perc +geo_other_perc
                   + meta_certified_est_bldg + meta_certified_est_land + ind_arms_length
                   + as.factor(char_bsmt) + as.factor(char_bsmt_fin) + as.factor(char_ext_wall)
                   + as.factor(char_gar1_size) + as.factor(char_heat) + as.factor(char_oheat) + geo_school_hs_district
                   + as.factor(char_roof_cnst) + as.factor(char_use) + as.factor(char_cnst_qlty) + as.factor(ind_garage) + as.factor(char_bsmt_fin), data = train.df, mtry = 4, 
                   nodesize = 5, ntree = 50)

# predicted classes 
rf.pred <- predict(rf, test.df, type="class")

# MSE in the test set 
mean((test.df$sale_price-rf.pred)^2)





