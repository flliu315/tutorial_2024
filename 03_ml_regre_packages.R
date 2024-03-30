# --------------------------------------------
# Script Name: machine learning for regression
# Purpose: Here is script for creating a regression model with
#          machine learning on data. The goal is to allow every
#          student to understand the syntax of each package.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-29
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################
# 01-regression tree model (rpart)
##################################################
# https://rpubs.com/anshulkumar/KNN-DecTree-RandForest

# Load data
# the data are downloaded from the website
# https://github.com/QCBSRworkshops/workshop04/tree/main/book-en/data
data <- read.csv("data/MLdata/dickcissel.csv", 
                 stringsAsFactors = TRUE)

df <- na.omit(data) # Remove missing observations
head(df) 

##-------------------------------------------
# pre-Processing data
# A) scaled = (x – x̄) / s
str(df)
df[3:ncol(data)] <- scale(df[3:ncol(df)]) # scale all but target
head(df)

# B) conversion to dummy (binary) variables

df <- fastDummies::dummy_cols(df, remove_first_dummy = TRUE,  
                 remove_selected_columns = TRUE)

# c) splitting df into training and test datasets

trainingRowIndex <- sample(1:nrow(df), 0.75*nrow(df)) 
dtrain <- df[trainingRowIndex, ]  # model training data
dtest  <- df[-trainingRowIndex, ]   # test data

##-----------------------------------------------
# create a decision tree 
library(rpart) 
library(rpart.plot)
dt_tree <- rpart(abund ~ ., data=dtrain, method = 'anova')
rpart.plot(dt_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

##----------------------------------------------------
# Variable importance
library(caret) # for calculating feature importance
(varimp.dt_tree <- varImp(dt_tree))

##--------------------------------------------------
# evaluate model using test dataset

dtest$dt_tree.pred <- predict(dt_tree, newdata = dtest)
head(dtest[c("abund","dt_tree.pred")], n=nrow(dtest))
Metrics::rmse(dtest$abund,dtest$dt_tree.pred)

##############################################
## 02-random forest regression
##############################################
# https://rpubs.com/anshulkumar/KNN-DecTree-RandForest

# load data and pre-process data as above

# make a randomFores regression
library(randomForest)
rf_tree <- randomForest(abund ~ .,data=dtrain, 
                    proximity=TRUE,
                    ntree=1000)
# Variable importance
(varimp.rf_tree <- caret::varImp(rf_tree))

# evaluate the model

dtest$rf_tree.pred <- predict(rf_tree, newdata = dtest)
Metrics::rmse(dtest$abund, dtest$rf_tree.pred)

##############################################
## 03-boosting regression tree
##############################################
# https://www.geeksforgeeks.org/boosting-in-r/

# load data and pre-process data as above

# make a boosting regression tree

library(gbm)
gbm_tree <- gbm(abund ~ ., data = dtrain,
             distribution = "gaussian",
             n.trees = 5000, shrinkage = 0.01,
             interaction.depth = 4,
             bag.fraction = 0.7,
             n.minobsinnode = 5)

# evaluate the model

dtest$gbm_tree.pred <- predict(gbm_tree, newdata = dtest)
Metrics::rmse(dtest$abund, dtest$gbm_tree.pred)

# the above code is to train tree models with different 
# packages and achieves a good result with boosting tree 
# algorithm. the below is to show the resembling method 
# in a framework of caret with different algorithms.
# 
########################################
## 04-simultaneously build tree models with 
## several algorithms using caret package
########################################
# https://towardsdatascience.com/create-predictive-models-in-r-with-caret-12baf9941236

# first take a look at the algorithms
modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames

modelLookup("rpart")
modelLookup("rf")
modelLookup("gbm")

# load data
data <- read.csv("data/MLdata/dickcissel.csv", 
                 stringsAsFactors = TRUE)
# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$abund, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data[trainIndex,]
data_test <- data[-trainIndex,]

# A) training a tree regression

# Self-defining the followings for optional a model 
# before training 

# 1) self-defining pre-processing of training data

# pre-processing usually includes center and scale data
# by defining preProcess = c('scale', 'center'), and put  
# it into train()

# 2) self-definig trControl for resampling process, and 
# put it outside train() but cite it in train() by the
# parameter trControl

fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 2)    # repeated ten times

# 3) self-defining way for finding hyperparameters 

# there ways include tunelength (automatically),
# tuneGrid (manually) and search = “random”, and 
# select one in train()

# Train a decision tree model
model_rpart <- train(abund ~ ., data = data_train, 
               method = "rpart",
               trControl = fitControl,
               preProcess = c('scale', 'center'),
               tuneLength = 5,# find an optimal cp based on its 5 values
               metric="RMSE") 

# Predict on the test data
predictions_rpart <- predict(model_rpart, newdata = data_test)

# evaluate regression performance
Metrics::rmse(data_test$abund, predictions_rpart)

# B) training a rf regression

model_rf <- train(abund ~ ., data = data_train, 
                     method = "rf",
                     trControl = fitControl,
                     preProcess = c('scale', 'center'),
                     tuneLength = 5,
                     metric="RMSE") 

predictions_rf <- predict(model_rf, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_rf)

# C) training a boosting regression

model_gbm <- train(abund ~ ., data = data_train, 
                     method = "gbm",
                     trControl = fitControl,
                     preProcess = c('scale', 'center'),
                     tuneLength = 5,
                     metric="RMSE")  

predictions_gbm <- predict(model_gbm, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_gbm)

# Compare the models' performances for final picking
models_compare <- resamples(list(TREE=model_rpart, 
                                 RF=model_rf, 
                                 GBM=model_gbm))
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free"))
bwplot(models_compare, scales=scales)
