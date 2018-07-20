# Model 4 - RANDOM FOREST MODEL

train <- read.csv(file.choose())
test <- read.csv(file.choose())
test$SalePrice <- NA

####
convert <- c('BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr',
             'OverallQual','OverallCond','TotRmsAbvGrd','Fireplaces','GarageCars','MoSold')

train[,convert] <- data.frame(apply(train[convert],2, as.factor))
test[,convert] <- data.frame(apply(test[convert],2, as.factor))

###
conv <- sapply(train, is.factor)
train[conv] <- lapply(train[conv], as.character)

conv1 <- sapply(test, is.factor)
test[conv1] <- lapply(test[conv1], as.character)

str(train)

# Assigning 1 to test data and 0 to train data. (Helps in seperating data easily)

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))

# Combining Train & Test Dataset

fullSet <- rbind(test,train)

# Converting back to a factor

conv2 <- sapply(fullSet, is.character)
fullSet[conv2] <- lapply(fullSet[conv2], as.factor)
str(fullSet)

# To ensure that both the test and train sets have the same levels. 

test.new <- fullSet[fullSet$isTest==1,]
train.new <- fullSet[fullSet$isTest==0,]
#train.new$SalePrice_log <- log(train.new$SalePrice)
## Variables Selected

train1 <- train.new[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                       "LandSlope","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual","OverallCond",
                       "YearBuilt","YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                       "MasVnrArea","ExterQual","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
                       "BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","HeatingQC","CentralAir","Electrical",
                       "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                       "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Fireplaces","FireplaceQu",
                       "GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond",
                       "PavedDrive","WoodDeckSF","OpenPorchSF","Fence","SaleCondition","SalePrice")]

test1 <- test.new[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                     "LandSlope","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual","OverallCond",
                     "YearBuilt","YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                     "MasVnrArea","ExterQual","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
                     "BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","HeatingQC","CentralAir","Electrical",
                     "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                     "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Fireplaces","FireplaceQu",
                     "GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond",
                     "PavedDrive","WoodDeckSF","OpenPorchSF","Fence","SaleCondition")]

str(train1)
str(test1)

## Creating a dataset to validate on
vdata_rf <- train1[sample(seq_len(nrow(train1)), size = dim(train1)[1]*0.25),]

## Hence, actual train data would turn into 

train_rf <- train1[-sample(seq_len(nrow(train1)), size = dim(train1)[1]*0.25),]

library("randomForest")
set.seed(370)

fitrfmodel = randomForest(SalePrice ~.,data = train_rf, 
                          ntree = 700, 
                          importance = TRUE)
summary(fitrfmodel)

par(mfrow=c(1,1))
plot(fitrfmodel)
# K-FOLD cross validation of the model

set.seed(1)
tune.out = tune(randomForest, SalePrice~., data = train_rf, kernel ="linear",
                ranges = list(cost  = c(0.001, 0.01, 0.1, 1, 5, 10,100)))

summary(tune.out)

# best parameters:
 # cost = 0.01

#- best performance: 920909032 

#- Detailed performance results:
#  cost     error dispersion
#1 1e-03 939716869  646227810
#2 1e-02 920909032  630544575
#3 1e-01 931253323  644772855
#4 1e+00 939320319  652097074
#5 5e+00 925652699  626478435
#6 1e+01 938260003  656936793
#7 1e+02 940700235  650762586

# Best Model
bestmod_rf = tune.out$best.model
summary(bestmod_rf)
plot(bestmod_rf)

# validate model:
predrf = predict(bestmod_rf, vdata_rf)
xdf = data.frame(SalePrice = predrf, oldprice = vdata_rf$SalePrice)
xdf$err_rate = sqrt(abs((xdf$SalePrice^2) - (xdf$oldprice^2)))
xdf$pcterr = abs((xdf$oldprice - xdf$SalePrice )/xdf$oldprice)*100

median(xdf$err_rate)  # median prediction error in $ = 39599.94$ // 0.8466926
median(xdf$pcterr)    # median prediction error as % difference from correct value = 2.954111% // 0.2499417 %


## Accuracy of the model on validation dataset
#library(caret)
#testing.probs <- predict(fitrfmodel, vdata_rf, type ='class')
#pred.logit <- rep('0',length(testing.probs))
#pred.logit[testing.probs>= 0.5]  ='1'
#confusionMatrix(vdata_rf$SalePrice, pred.logit)

validtestdf_rf = data.frame(trueval = vdata_rf$SalePrice,linval = predrf)
library(rcompanion)
plot(trueval~linval,data = validtestdf_rf)
cor(validtestdf_rf)                                ## 0.9659179 // 0.9738219

#Prediction

predrf = predict(bestmod_rf, test1)
predmodel = data.frame(Id = test$Id, SalePrice = predrf)

write.csv(predmodel, "3rd Model_RF.csv", row.names = FALSE)