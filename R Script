setwd("C:/Users/lenovo/Documents")

#Loading packages 
library(tidyverse)
library(caret)
library(ggplot2)
library(mice)
library(dplyr)
library(readxl)
library(glmnet)


#load train and test datasets
train <- read.csv(file.choose(), header = TRUE, sep= ',', stringsAsFactors = FALSE)
test <- read.csv(file.choose(), header = TRUE, sep= ',', stringsAsFactors = FALSE)

str(train)
str(test)

#Add a new col in test daatset
test$SalePrice <- colnames(test)[81]
test

#Data Preparation
#Calculate the missing data in train and test datasets
sum(is.na(train))/(nrow(train) * ncol(train))
sum(is.na(test))/(nrow(test) * ncol(test))

#check for unique train and test datasets
nrow(train) - nrow(unique(train))
nrow(test) - nrow(unique(test))

#Feature engg: converting all the training categorical data into factor
train$MSZoning <- as.factor(train$MSZoning)
train$Street <- as.factor(train$Street)
train$Alley <- as.factor(train$Alley)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)
train$Utilities <- as.factor(train$Utilities)
train$LotConfig <- as.factor(train$LotConfig)
train$LandSlope <- as.factor(train$LandSlope)
train$Neighborhood <- as.factor(train$Neighborhood)
train$Condition1 <- as.factor(train$Condition1)
train$Condition2 <- as.factor(train$Condition2)
train$BldgType <- as.factor(train$BldgType)
train$HouseStyle <- as.factor(train$HouseStyle)
train$RoofStyle <- as.factor(train$RoofStyle)
train$RoofMatl <- as.factor(train$RoofMatl)
train$Exterior1st <- as.factor(train$Exterior1st)
train$Exterior2nd <- as.factor(train$Exterior2nd)
train$MasVnrType <- as.factor(train$MasVnrType)
train$ExterQual <- as.factor(train$ExterQual)
train$ExterCond <- as.factor(train$ExterCond)
train$Foundation <- as.factor(train$Foundation)
train$BsmtQual <- as.factor(train$BsmtQual)
train$BsmtCond <- as.factor(train$BsmtCond)
train$BsmtExposure <- as.factor(train$BsmtExposure)
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)
train$Heating <- as.factor(train$Heating)
train$HeatingQC <- as.factor(train$HeatingQC)
train$CentralAir <- as.factor(train$CentralAir)
train$Electrical <- as.factor(train$Electrical)
train$KitchenQual <- as.factor(train$KitchenQual)
train$Functional <- as.factor(train$Functional)
train$FireplaceQu <- as.factor(train$FireplaceQu)
train$GarageType <- as.factor(train$GarageType)
train$GarageFinish <- as.factor(train$GarageFinish)
train$GarageQual <- as.factor(train$GarageQual)
train$GarageCond <- as.factor(train$GarageCond)
train$PavedDrive <- as.factor(train$PavedDrive)
train$PoolQC <- as.factor(train$PoolQC)
train$MiscFeature <- as.factor(train$MiscFeature)
train$Fence <- as.factor(train$Fence)
train$SaleType <- as.factor(train$SaleType)
train$SaleCondition <- as.factor(train$SaleCondition)

#Feature engg: converting all the testing categorical data into factor
test$MSZoning <- as.factor(test$MSZoning)
test$Street <- as.factor(test$Street)
test$Alley <- as.factor(test$Alley)
test$LotShape <- as.factor(test$LotShape)
test$LandContour <- as.factor(test$LandContour)
test$Utilities <- as.factor(test$Utilities)
test$LotConfig <- as.factor(test$LotConfig)
test$LandSlope <- as.factor(test$LandSlope)
test$Neighborhood <- as.factor(test$Neighborhood)
test$Condition1 <- as.factor(test$Condition1)
test$Condition2 <- as.factor(test$Condition2)
test$BldgType <- as.factor(test$BldgType)
test$HouseStyle <- as.factor(test$HouseStyle)
test$RoofStyle <- as.factor(test$RoofStyle)
test$RoofMatl <- as.factor(test$RoofMatl)
test$Exterior1st <- as.factor(test$Exterior1st)
test$Exterior2nd <- as.factor(test$Exterior2nd)
test$MasVnrType <- as.factor(test$MasVnrType)
test$ExterQual <- as.factor(test$ExterQual)
test$ExterCond <- as.factor(test$ExterCond)
test$Foundation <- as.factor(test$Foundation)
test$BsmtQual <- as.factor(test$BsmtQual)
test$BsmtCond <- as.factor(test$BsmtCond)
test$BsmtExposure <- as.factor(test$BsmtExposure)
test$BsmtFinType1 <- as.factor(test$BsmtFinType1)
test$BsmtFinType2 <- as.factor(test$BsmtFinType2)
test$Heating <- as.factor(test$Heating)
test$HeatingQC <- as.factor(test$HeatingQC)
test$CentralAir <- as.factor(test$CentralAir)
test$Electrical <- as.factor(test$Electrical)
test$KitchenQual <- as.factor(test$KitchenQual)
test$Functional <- as.factor(test$Functional)
test$FireplaceQu <- as.factor(test$FireplaceQu)
test$GarageType <- as.factor(test$GarageType)
test$GarageFinish <- as.factor(test$GarageFinish)
test$GarageQual <- as.factor(test$GarageQual)
test$GarageCond <- as.factor(test$GarageCond)
test$PavedDrive <- as.factor(test$PavedDrive)
test$PoolQC <- as.factor(test$PoolQC)
test$MiscFeature <- as.factor(test$MiscFeature)
test$Fence <- as.factor(test$Fence)
test$SaleType <- as.factor(test$SaleType)
test$SaleCondition <- as.factor(test$SaleCondition)

#Feature engg: converting all the training integer type data into numerical variable. 
train$MSSubClass <- as.numeric(train$MSSubClass)
train$LotFrontage <- as.numeric(train$LotFrontage)
train$LotArea <- as.numeric(train$LotArea)
train$OverallQual <- as.numeric(train$OverallQual)
train$OverallCond <- as.numeric(train$OverallCond)
train$YearBuilt <- as.numeric(train$YearBuilt)
train$YearRemodAdd <- as.numeric(train$YearRemodAdd)
train$BsmtFinSF1 <- as.numeric(train$BsmtFinSF1)
train$BsmtFinSF2 <- as.numeric(train$BsmtFinSF2)
train$BsmtUnfSF <- as.numeric(train$BsmtUnfSF)
train$TotalBsmtSF <- as.numeric(train$TotalBsmtSF)
train$X1stFlrSF <- as.numeric(train$X1stFlrSF)
train$X2ndFlrSF <- as.numeric(train$X2ndFlrSF)
train$LowQualFinSF <- as.numeric(train$LowQualFinSF)
train$GrLivArea <- as.numeric(train$GrLivArea)
train$BsmtFullBath <- as.numeric(train$BsmtFullBath)
train$BsmtHalfBath <- as.numeric(train$BsmtHalfBath)
train$FullBath <- as.numeric(train$FullBath)
train$HalfBath <- as.numeric(train$HalfBath)
train$BedroomAbvGr <- as.numeric(train$BedroomAbvGr)
train$KitchenAbvGr <- as.numeric(train$KitchenAbvGr)
train$TotRmsAbvGrd <- as.numeric(train$TotRmsAbvGrd)
train$Fireplaces <- as.numeric(train$Fireplaces)
train$GarageCars <- as.numeric(train$GarageCars)
train$GarageArea <- as.numeric(train$GarageArea)
train$WoodDeckSF <- as.numeric(train$WoodDeckSF)
train$OpenPorchSF <- as.numeric(train$OpenPorchSF)
train$EnclosedPorch <- as.numeric(train$EnclosedPorch)
train$X3SsnPorch <- as.numeric(train$X3SsnPorch)
train$ScreenPorch <- as.numeric(train$ScreenPorch)
train$PoolArea <- as.numeric(train$PoolArea)
train$MiscVal <- as.numeric(train$MiscVal)
train$MoSold <- as.numeric(train$MoSold)
train$YrSold <- as.numeric(train$YrSold)
train$SalePrice <- as.numeric(train$SalePrice)

#Feature engg: converting all the testing integer type data into numerical. 
test$MSSubClass <- as.numeric(test$MSSubClass)
test$LotFrontage <- as.numeric(test$LotFrontage)
test$LotArea <- as.numeric(test$LotArea)
test$OverallQual <- as.numeric(test$OverallQual)
test$OverallCond <- as.numeric(test$OverallCond)
test$YearBuilt <- as.numeric(test$YearBuilt)
test$YearRemodAdd <- as.numeric(test$YearRemodAdd)
test$BsmtFinSF1 <- as.numeric(test$BsmtFinSF1)
test$BsmtFinSF2 <- as.numeric(test$BsmtFinSF2)
test$BsmtUnfSF <- as.numeric(test$BsmtUnfSF)
test$TotalBsmtSF <- as.numeric(test$TotalBsmtSF)
test$X1stFlrSF <- as.numeric(test$X1stFlrSF)
test$X2ndFlrSF <- as.numeric(test$X2ndFlrSF)
test$LowQualFinSF <- as.numeric(test$LowQualFinSF)
test$GrLivArea <- as.numeric(test$GrLivArea)
test$BsmtFullBath <- as.numeric(test$BsmtFullBath)
test$BsmtHalfBath <- as.numeric(test$BsmtHalfBath)
test$FullBath <- as.numeric(test$FullBath)
test$HalfBath <- as.numeric(test$HalfBath)
test$BedroomAbvGr <- as.numeric(test$BedroomAbvGr)
test$KitchenAbvGr <- as.numeric(test$KitchenAbvGr)
test$TotRmsAbvGrd <- as.numeric(test$TotRmsAbvGrd)
test$Fireplaces <- as.numeric(test$Fireplaces)
test$GarageCars <- as.numeric(test$GarageCars)
test$GarageArea <- as.numeric(test$GarageArea)
test$WoodDeckSF <- as.numeric(test$WoodDeckSF)
test$OpenPorchSF <- as.numeric(test$OpenPorchSF)
test$EnclosedPorch <- as.numeric(test$EnclosedPorch)
test$X3SsnPorch <- as.numeric(test$X3SsnPorch)
test$ScreenPorch <- as.numeric(test$ScreenPorch)
test$PoolArea <- as.numeric(test$PoolArea)
test$MiscVal <- as.numeric(test$MiscVal)
test$MoSold <- as.numeric(test$MoSold)
test$YrSold <- as.numeric(test$YrSold)
test$SalePrice <- as.numeric(test$SalePrice)

#Feature engg- all the "NA" in the categorical variables were replaced by "No Access" or "None" in the testing and training datasets
train$Alley[is.na(train$Alley)] = "No alley access"
test$Alley[is.na(test$Alley)] = "No alley access"
train$BsmtQual[is.na(train$BsmtQual)] = "No basement"
test$BsmtQual[is.na(test$BsmtQual)] = "No basement"
train$BsmtCond[is.na(train$BsmtCond)] = "No basement"
test$BsmtCond[is.na(test$BsmtCond)] = "No basement"
train$BsmtExposure[is.na(train$BsmtExposure)] = "No basement"
test$BsmtExposure[is.na(test$BsmtExposure)] = "No basement"
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "No basement"
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "No basement"
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "No basement"
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "No basement"
train$FireplaceQu[is.na(train$FireplaceQu)] = "No fireplace"
test$FireplaceQu[is.na(test$FireplaceQu)] = "No fireplace"
train$GarageType[is.na(train$GarageType)] = "No garage"
test$GarageType[is.na(test$GarageType)] = "No garage"
train$GarageFinish[is.na(train$GarageFinish)] = "No garage"
test$GarageFinish[is.na(test$GarageFinish)] = "No garage"
train$GarageQual[is.na(train$GarageQual)] = "No garage"
test$GarageQual[is.na(test$GarageQual)] = "No garage"
train$GarageCond[is.na(train$GarageCond)] = "No garage"
test$GarageCond[is.na(test$GarageCond)] = "No garage"
train$PoolQC[is.na(train$PoolQC)] = "No pool"
test$PoolQC[is.na(test$PoolQC)] = "No pool"
train$Fence[is.na(train$Fence)] = "No fence"
test$Fence[is.na(test$Fence)] = "No fence"
train$MiscFeature[is.na(train$MiscFeature)] = "None"
test$MiscFeature[is.na(test$MiscFeature)] = "None"
train$MasVnrType[is.na(train$MasVnrType)] = "None"
test$MasVnrType[is.na(test$MasVnrType)] = "None"

#Feature Engg: For some of the categorical variables missing observations were replaced with frequently occuring variables in the testing and training datasets
train$Electrical[is.na(train$Electrical)] = "SBrkr"
test$MSZoning[is.na(test$MSZoning)] = "RL"
test$Utilities[is.na(test$Utilities)] = "AllPub"
test$Exterior1st[is.na(test$Exterior1st)] = "VinylSd"
test$Exterior2nd[is.na(test$Exterior2nd)] = "VinylSd"
test$KitchenQual[is.na(test$KitchenQual)] = "TA"
test$Functional[is.na(test$Functional)] = "Min2"
test$SaleType[is.na(test$SaleType)] = "WD"

#Feature Engg: For the numerical variables missing observations were replaced by '0' in the training and testing datasets
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0
test$GarageCars[is.na(test$GarageCars)] = 0
test$GarageArea[is.na(test$GarageArea)] = 0
train$GarageYrBlt[is.na(train$GarageYrBlt)] = 0
test$GarageYrBlt[is.na(test$GarageYrBlt)] = 0
train$MasVnrArea[is.na(train$MasVnrArea)] = 0
test$MasVnrArea[is.na(test$MasVnrArea)] = 0

#Feature Engg: Imputation method was applied to few variables in teh training and testing datasets
train$LotFrontage[is.na(train$LotFrontage)] = median(train$LotFrontage, na.rm = TRUE)
test$LotFrontage[is.na(test$LotFrontage)] = median(test$LotFrontage, na.rm = TRUE)

#Seed
set.seed(42)

#Correlation between SalePrice and other numerical variables in the train dataset
cor.test(train$SalePrice, train$MSSubClass, method = c("pearson")) #Sig Negative Correlation
cor.test(train$SalePrice, train$LotFrontage, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$LotArea, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$OverallQual, method = c("pearson")) #Sis positive correlation
cor.test(train$SalePrice, train$OverallCond, method = c("pearson")) #Sig negative correlation
cor.test(train$SalePrice, train$YearBuilt, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$YearRemodAdd, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$MasVnrArea, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$BsmtFinSF1, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$BsmtFinSF2, method = c("pearson")) #Not sig
cor.test(train$SalePrice, train$BsmtUnfSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$TotalBsmtSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$X1stFlrSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$X2ndFlrSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$LowQualFinSF, method = c("pearson")) #Not Sig
cor.test(train$SalePrice, train$GrLivArea, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$BsmtFullBath, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$BsmtHalfBath, method = c("pearson")) #Not Sig
cor.test(train$SalePrice, train$BedroomAbvGr, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$KitchenAbvGr, method = c("pearson")) #Sig negative correlation
cor.test(train$SalePrice, train$TotRmsAbvGrd, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$Fireplaces, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$GarageYrBlt, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$GarageCars, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$GarageArea, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$WoodDeckSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$OpenPorchSF, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$EnclosedPorch, method = c("pearson")) #Sig negative correlation
cor.test(train$SalePrice, train$X3SsnPorch, method = c("pearson")) #Not Sig 
cor.test(train$SalePrice, train$ScreenPorch, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$PoolArea, method = c("pearson")) #Sig positive correlation
cor.test(train$SalePrice, train$MiscVal, method = c("pearson")) #Not Sig
cor.test(train$SalePrice, train$MoSold, method = c("pearson")) #Not Sig 
cor.test(train$SalePrice, train$YrSold, method = c("pearson")) #Not Sig 

#---------------------------------------------------------------------------------
#Linear regression model
model_lm <- lm(SalePrice ~ ., data = train)
summary(model_lm) #Adj R sqr = 0.9193

#Predict the test dataset
pred_lm <- predict(model_lm, test)
pred_lm

#Calculating errors
RMSE(pred_lm, test$SalePrice, na.rm = FALSE)
MAE(pred_lm, test$SalePrice, na.rm = FALSE) 

#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------

#Model log interactions

model_log_interactions <- lm(log(SalePrice) ~ log(OverallQual) * BsmtQual * KitchenQual * GarageQual + ., data = train)
summary(model_log_interactions) #Adj R sqr = 0.9332


#Predict the test dataset
pred_log_interactions <- exp(predict(model_log_interactions, test))
pred_log_interactions

#Calculating errors
RMSE(pred_log_interactions, test$SalePrice, na.rm = FALSE) 
MAE(pred_log_interactions, test$SalePrice, na.rm = FALSE) 

#---------------------------------------------------------------------------------

#Combine test and train datasets into complete datasets
house <- merge(train, test, all.x = TRUE, all.y = TRUE)
house

#---------------------------------------------------------------------------------

#Model - Lasso Regularization
library(glmnet)

#create y variable and matrix of x variable
y <- log(train$SalePrice)
x <- model.matrix(Id ~ OverallQual * Neighborhood * BldgType + sqrt(OverallQual) * Neighborhood * BldgType + log(OverallQual) * Neighborhood * BldgType, data = house)
x <- cbind(house$Id, x)

#Splitting x into training, and testing
x.training <- subset(x, x[ ,1] <= 1460)
x.testing <- subset(x, x[ ,1] >= 1461)

#LASSO alpha = 1
model_lasso <- glmnet(x = x.training, y = y, alpha = 1)
plot(model_lasso, xvar = "lambda")

#select best penalty lambda
crossval <- cv.glmnet(x = x.training, y = y, alpha = 1)
plot(crossval)
penalty.lasso <- crossval$lambda.min
log(penalty.lasso)
plot(crossval, xlim = c(-8.5, -6), ylim = c(.006, .008))
lasso.opt.file <- glmnet(x = x.training, y = y, alpha = 1, lambda = penalty.lasso)
coeff(lasso.opt.file)

#predicting performance on test dataset
lasso.testing <- exp(predict(lasso.opt.file, S = penalty.lasso, newx = x.testing))
lasso.testing

#Calculating errors
RMSE(lasso.testing, test$SalePrice, na.rm = FALSE) 
MAE(lasso.testing, test$SalePrice, na.rm = FALSE) 
#---------------------------------------------------------------------------------

#Creating csv file with predicted sale prices for submission

submit <- data.frame(Id = test$Id, SalePrice = lasso.testing)
write.csv(submit, file = "Prediction_lasso.csv", row.names = FALSE)
rm(submit)
#---------------------------------------------------------------------------------

#Model - Ridge Regularization

#Ridge regression alpha = 0
model_ridge <- glmnet(x = x.training, y = y, alpha = 0)
plot(model_ridge, xvar = "lambda")

#select best penalty lambda
crossval <- cv.glmnet(x = x.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min
log(penalty.ridge)
plot(crossval, xlim = c(-8.5, -6), ylim = c(.006, .008))
ridge.opt.file <- glmnet(x = x.training, y = y, alpha = 0, lambda = penalty.ridge)
coeff(ridge.opt.file)

#predicting performance on test dataset
ridge.testing <- exp(predict(ridge.opt.file, S = penalty.ridge, newx = x.testing))
ridge.testing

#Calculating errors
RMSE(lasso.testing, test$SalePrice, na.rm = FALSE) 
MAE(lasso.testing, test$SalePrice, na.rm = FALSE) 
#---------------------------------------------------------------------------------

#Creating csv file with predicted sale prices for submission

submit <- data.frame(Id = test$Id, SalePrice = ridge.testing)
write.csv(submit, file = "Prediction_ridge.csv", row.names = FALSE)
rm(submit)
#---------------------------------------------------------------------------------

#Model - random forest model

library(caret)
library(dplyr)
install.packages("randomForest")
library(randomForest)

rf1 <- randomForest(SalePrice ~ ., train, ntree = 50, norm.votes = FALSE)
summary(rf1)
varImpPlot(rf1)

pred_rf1 <- predict(rf1, test)

#Calculating errors
RMSE(pred_rf1, test$SalePrice, na.rm = FALSE) 
MAE(pred_rf1, test$SalePrice, na.rm = FALSE) 
#---------------------------------------------------------------------------------

#Creating csv file with predicted sale prices for submission
submit <- data.frame(Id = test$Id, SalePrice = pred_rf1)

write.csv(submit, file = "Prediction_rf.csv", row.names = FALSE)
rm(submit)

#---------------------------------------------------------------------------------

#Model- Gradient Boosting method

install.packages("gbm")
library(gbm)

require(gbm)

gbm1 <- gbm(SalePrice ~ ., train, distribution = "gaussian",n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(gbm1)

pred_gbm1 <- predict(gbm1, test)

#Calculating errors
RMSE(pred_gbm1, test$SalePrice, na.rm = FALSE) 
MAE(pred_gbm1, test$SalePrice, na.rm = FALSE) 
#---------------------------------------------------------------------------------

#Creating csv file with predicted sale prices for submission
submit <- data.frame(Id = test$Id, SalePrice = pred_gbm1)

write.csv(submit, file = "Prediction_gbm.csv", row.names = FALSE)
rm(submit)

#---------------------------------------------------------------------------------







