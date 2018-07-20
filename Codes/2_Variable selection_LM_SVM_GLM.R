## VARIABLE SELECTION
## Data Loading

train <- read.csv(file.choose())         # Load "ames_train_cleaned1.csv"
test <- read.csv(file.choose())          # Load "ames_test_cleaned1.csv"
train$SalePrice_log <- log(train$SalePrice)
str(train)

par(mar=c(7,5,1,1))
boxplot(train[1:5,6:80], las=2)
boxplot(test, las=2)
# Checking if any numeric variables can be converted to categorical

num_level <- sapply(train, is.numeric)
str(train[num_level])

train$BsmtFullBath
train$BsmtHalfBath
train$FullBath
train$HalfBath
train$BedroomAbvGr
train$KitchenAbvGr
train$OverallQual
train$OverallCond
train$TotRmsAbvGrd
train$Fireplaces
train$GarageCars 
train$MoSold

convert <- c('BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr',
             'OverallQual','OverallCond','TotRmsAbvGrd','Fireplaces','GarageCars','MoSold')

train[,convert] <- data.frame(apply(train[convert],2, as.factor))
test[,convert] <- data.frame(apply(test[convert],2, as.factor))
str(test)

# Data Visualization
# Univariate Analysis

library(ggplot2)
ggplot(train, aes(SalePrice)) + geom_histogram(bins =30)

# Clearly Graph is skewed towards left
# Using log transformation

#train$SalePrice_log <- log(train$SalePrice)
ggplot(train, aes(SalePrice_log)) + geom_histogram(bins =30)

# Hence, SalePrice is normally disributed when transformed into log. 

#Boxplot of Categorical variable Foundation

ggplot(train, aes(x=reorder(Foundation ,Foundation, function(x)-length(x)))) + 
  geom_bar() + geom_bar(fill = "skyblue") +
  xlab('Foundation') + ylab('Count') + 
  ggtitle('Types of Foundation Mostly Used')
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Bivariate Analysis

#Finding relationship between Total Living Area and Sale Price & log of SalePrice.
  
options(repr.plot.width=5, repr.plot.height=5)
ggplot(train, aes(GrLivArea, SalePrice)) + geom_point(colour = "skyblue") +
  xlab('Total Living Area (Sqft)') + ylab('Sale Price ($)') + 
  ggtitle('Relationship between Total Living Area and Sale Price')


options(repr.plot.width=5, repr.plot.height=5)
ggplot(train, aes(GrLivArea, SalePrice_log)) + geom_point(colour = "skyblue") +
  xlab('Total Living Area (Sqft)') + ylab('Log ofSale-Price') + 
  ggtitle('Relationship between Total Living Area and SalePrice_log')


# Relationship between Houses built & Garages built.
options(repr.plot.width=6.5, repr.plot.height=5)
ggplot(train, aes(YearBuilt, GarageYrBlt)) + geom_point(colour = "cyan4",alpha = 0.5) +
  xlab('Year Houses Built') + ylab('Year Garages Built') + 
  ggtitle('Relationship between Houses built & Garages built')


# Relationship between Neighborhood & Sale-Prices
options(repr.plot.width=6.5, repr.plot.height=5)
ggplot(train, aes(Neighborhood, SalePrice)) + geom_point(aes(color = factor(Neighborhood)), alpha = 0.3) +
  xlab('Neighborhood') + ylab('Sale-Price') + 
  ggtitle('Relationship between Neighborhood & Sale-Prices')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Applying Boruta Feature Selection algorithm (Uses Random Forest iteration method)

set.seed(123)
library(Boruta)
boruta.train <- Boruta(SalePrice~.-Id, data = train, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

getSelectedAttributes(final.boruta)

## Variables selected

train1 <- train[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                   "LandSlope","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual","OverallCond",
                   "YearBuilt","YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                   "MasVnrArea","ExterQual","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
                   "BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","HeatingQC","CentralAir","Electrical",
                   "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                   "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Fireplaces","FireplaceQu",
                   "GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond",
                   "PavedDrive","WoodDeckSF","OpenPorchSF","Fence","SaleCondition","SalePrice","SalePrice_log")]

test1 <- test[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                 "LandSlope","Neighborhood","Condition1","BldgType","HouseStyle","OverallQual","OverallCond",
                 "YearBuilt","YearRemodAdd","RoofStyle","Exterior1st","Exterior2nd","MasVnrType",
                 "MasVnrArea","ExterQual","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
                 "BsmtFinSF1","BsmtUnfSF", "TotalBsmtSF","HeatingQC","CentralAir","Electrical",
                 "X1stFlrSF","X2ndFlrSF","GrLivArea","BsmtFullBath","FullBath","HalfBath",
                 "BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Fireplaces","FireplaceQu",
                 "GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond",
                 "PavedDrive","WoodDeckSF","OpenPorchSF","Fence","SaleCondition")]

str(train1)
summary(train1)

####################################################################################################
## Model Building 

## 1 - LINEAR MODEL  ##

## Creating a dataset to validate on
vdata_lm <- train1[sample(seq_len(nrow(train1)), size = dim(train1)[1]*0.25),]

## Hence, actual train data would turn into 

train_lm <- train1[-sample(seq_len(nrow(train1)), size = dim(train1)[1]*0.25),]

attach(train_lm)
str(train_lm)
boxplot(train_lm)
dim(train_lm)

## Factor to numeric
# Creating Dummy dataset

dtrain <- train_lm
facall <- sapply(dtrain, is.factor)
dtrain[facall] <- lapply(dtrain[facall], as.numeric)
str(dtrain)

dvdata <- vdata_lm
vfacall <- sapply(dvdata, is.factor)
dvdata[vfacall] <- lapply(dvdata[vfacall], as.numeric)

dtest <- test1
dtfacall <- sapply(dtest, is.factor)
dtest[dtfacall] <- lapply(dtest[dtfacall], as.numeric)

# Creating a Correlation Heatmap

library(corrplot)

M <- cor(dtrain)

cor.dtrain <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.dtrain(dtrain)

library(RColorBrewer)
corrplot(M, type="full", order="hclust",tl.cex = 0.6,
         col=brewer.pal(n=8, name="RdYlBu"),
         tl.col="black", tl.srt=90,
         p.mat = p.mat, sig.level = 0.01, insig = "blank")


## Model building considering all variables and outliers

dtrain.lm <- lm(dtrain)
dtrain.lm
summary(dtrain.lm)

#Residual standard error: 18.23 on 1037 degrees of freedom
#Multiple R-squared:  0.826,	Adjusted R-squared:  0.8164 
#F-statistic: 86.34 on 57 and 1037 DF,  p-value: < 2.2e-16

library(car)
vif(dtrain.lm)

## Stepwise Linear model considering outliers 

str(dtrain)
dtrain1.lm<- step(lm(SalePrice_log~.-SalePrice,data = dtrain),direction = "both")
summary(dtrain1.lm)

#Residual standard error: 0.14 on 1059 degrees of freedom
#Multiple R-squared:  0.8793,	Adjusted R-squared:  0.8754 
#F-statistic: 220.5 on 35 and 1059 DF,  p-value: < 2.2e-16

vif(dtrain1.lm)
library(lmtest)
ncvTest(dtrain.lm)
                    #Non-constant Variance Score Test 
                    #Variance formula: ~ fitted.values
                    #Chisquare = 131.874   Df = 1     p = 1.594199e-30

ncvTest(dtrain1.lm)

                    #Non-constant Variance Score Test 
                    #Variance formula: ~ fitted.values 
                    #Chisquare = 139.5652    Df = 1     p = 3.313616e-32 


## Residuals vs fitted Plot

par(mfrow=c(2,2))
plot(dtrain1.lm)
abline(dtrain1.lm)

# validation of model and error rates
fitmodel = data.frame(predict(dtrain1.lm, dvdata[-SalePrice], interval = "prediction"))
finalvalid = data.frame(SalePrice = fitmodel$fit, oldprice = dvdata$SalePrice_log)
finalvalid$err_rate = sqrt(abs((finalvalid$SalePrice^2) - (finalvalid$oldprice^2)))
finalvalid$pcterr = abs((finalvalid$oldprice - finalvalid$SalePrice )/finalvalid$oldprice)*100

help(abs)
median(finalvalid$err_rate)  # Median prediction error in $ = 65514.39$ / 1.318172
median(finalvalid$pcterr)    # Median prediction error as % difference from correct value = 7.91715% / 0.5990211

validtestdf_lm = data.frame(trueval = finalvalid$oldprice,linval = finalvalid$SalePrice)

library(rcompanion)
plot(trueval~linval,data = validtestdf_lm)
cor(validtestdf_lm)                             ## 0.9429055

# predict values for test
fitmodel_lm = data.frame(predict(dtrain1.lm, dtest, interval = "prediction"))
final_lm = data.frame(Id = test$Id, SalePrice = fitmodel_lm$fit)

write.csv(final_lm, "1st Model_linear.csv", row.names = FALSE)

####################################################################################################

## SUPPORT VECTOR MACHINE  ##

train_svm <- train1[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                       "LandSlope","Neighborhood","BldgType","OverallQual","OverallCond",
                       "YearBuilt","YearRemodAdd","RoofStyle","MasVnrType","MasVnrArea","ExterQual",
                       "Foundation","BsmtQual","BsmtExposure","BsmtFinType1", "BsmtFinSF1","BsmtUnfSF",
                       "TotalBsmtSF","HeatingQC","CentralAir","X1stFlrSF","X2ndFlrSF","GrLivArea",
                       "HalfBath","KitchenQual","TotRmsAbvGrd","GarageType","GarageYrBlt",
                       "GarageFinish", "GarageArea","PavedDrive","WoodDeckSF","OpenPorchSF",
                       "SaleCondition","SalePrice")]

test_svm <- test1[,c("MSSubClass","MSZoning","LotFrontage","LotArea","LotShape","LandContour",
                     "LandSlope","Neighborhood","BldgType","OverallQual","OverallCond",
                     "YearBuilt","YearRemodAdd","RoofStyle","MasVnrType","MasVnrArea","ExterQual",
                     "Foundation","BsmtQual","BsmtExposure","BsmtFinType1", "BsmtFinSF1","BsmtUnfSF",
                     "TotalBsmtSF","HeatingQC","CentralAir","X1stFlrSF","X2ndFlrSF","GrLivArea",
                     "HalfBath","KitchenQual","TotRmsAbvGrd","GarageType","GarageYrBlt",
                     "GarageFinish", "GarageArea","PavedDrive","WoodDeckSF","OpenPorchSF",
                     "SaleCondition")]

## Dummy dataset
vdata_svm <- train_svm[sample(seq_len(nrow(train_svm)), size = dim(train_svm)[1]*0.25),]

## Hence, actual train data would turn into 

train1_svm <- train_svm[-sample(seq_len(nrow(train_svm)), size = dim(train_svm)[1]*0.25),]

attach(train1_svm)

## SVM Model
library(e1071)
svm_model <- svm(SalePrice ~.,data = train1_svm, kernal ="linear")
summary(svm_model)
#Parameters:
        #  SVM-Type:  eps-regression 
        #  SVM-Kernel:  radial 
        #  cost:  1 
        #  gamma:  0.007142857 
        #  epsilon:  0.1 
        #  Number of Support Vectors:  693

# K-FOLD cross validation of model (7-folds)
set.seed(1)
tune.out = tune(svm, SalePrice~., data = train1_svm, kernel ="linear",
                ranges = list(cost  = c(0.001, 0.01, 0.1, 1, 5, 10,100)))

summary(tune.out)

  #cost 0.1 
  # best performance: 1002716578 

#Detailed performance results:
#  cost      error dispersion
#1 1e-03 1493511886  931254754
#2 1e-02 1122528142 1126742651
#3 1e-01 1002716578 1044739676
#4 1e+00 1039127995 1126526284
#5 5e+00 1051649024 1122233962
#6 1e+01 1053260127 1119252071
#7 1e+02 1102922650 1143847128

## Best model

bestmod = tune.out$best.model
summary(bestmod)

#Parameters:
          # SVM-Type:  eps-regression 
          # SVM-Kernel:  linear 
          # cost:  0.1 
          # gamma:  0.007142857
          # epsilon:  0.1 
          # Number of Support Vectors:  665

## Prediction

ypred  = predict(bestmod, vdata_svm)
table(predict = ypred, truth = vdata_svm$SalePrice)
err_rate_svm = sqrt(abs((ypred^2) - (vdata_svm$SalePrice^2)))
pcterr_svm = abs((vdata_svm$SalePrice - ypred )/vdata_svm$SalePrice)*100

median(err_rate_svm)  # median prediction error in $ = 55607.61$
median(pcterr_svm)    # median prediction error as % difference from correct value = 5.442585%

## Accuracy of the model on validation dataset

library(InformationValue)
confusionMatrix(ypred,vdata_svm$SalePrice)
validtestdf_svm = data.frame(trueval = vdata_svm$SalePrice,linval = ypred)

library(rcompanion)
plot(trueval~linval,data = validtestdf_svm)
cor(validtestdf_svm)                                   ##0.9573347

# Predict values for test
fitmodel_svm = data.frame(predict(bestmod, test_svm, interval = "prediction"))
final_svm = data.frame(Id = test$Id, SalePrice = fitmodel_svm$predict.bestmod..test_svm..interval....prediction..)

write.csv(final_svm, "2nd Model_SVM.csv", row.names = FALSE)

####################################################################################################

### GENERALISED LINEAR MODEL
## Creating a dataset to validate on

train_glm <- train1[,c("MSSubClass", "MSZoning","LotFrontage","LotArea","LandContour",
                       "BldgType","YearBuilt","YearRemodAdd","BsmtQual","BsmtExposure",
                       "BsmtFinType1","BsmtUnfSF","TotalBsmtSF","GrLivArea","HalfBath",
                       "KitchenQual","GarageType","GarageYrBlt","WoodDeckSF", "SaleCondition",
                       "SalePrice","SalePrice_log")]

test_glm <- test1[,c("MSSubClass", "MSZoning","LotFrontage","LotArea","LandContour",
                     "BldgType","YearBuilt","YearRemodAdd","BsmtQual","BsmtExposure",
                     "BsmtFinType1","BsmtUnfSF","TotalBsmtSF","GrLivArea","HalfBath",
                     "KitchenQual","GarageType","GarageYrBlt","WoodDeckSF", "SaleCondition")]

str(train_glm)
str(test_glm)

## Dummy dataset
vdata_glm <- train_glm[sample(seq_len(nrow(train_glm)), size = dim(train_glm)[1]*0.25),]

## Hence, actual train data would turn into 

train1_glm <- train_glm[-sample(seq_len(nrow(train_glm)), size = dim(train_glm)[1]*0.25),]

#

train.glm <- glm(SalePrice_log~.-SalePrice, data = train1_glm)
summary(train.glm)
# AIC: 26072  // -780.32

train1.glm <- step(glm(SalePrice_log~.-SalePrice ,data = train1_glm), direction = "both")
summary(train1.glm)
# AIC: 26064  // -788.94

# Validation checks:
predglm = predict(train1.glm, vdata_glm)
xdf = data.frame(SalePrice = predglm, oldprice = vdata_glm$SalePrice_log)
xdf$err_rate = sqrt(abs((xdf$SalePrice^2) - (xdf$oldprice^2)))
xdf$pcterr = abs((xdf$oldprice - xdf$SalePrice )/xdf$oldprice)*100

median(xdf$err_rate)  # median prediction error in $ = 66106.05$ // 1.423866
median(xdf$pcterr)    # median prediction error as % difference from correct value = 8.290854% // 0.718219
#

validtestdf_glm = data.frame(trueval = vdata_glm$SalePrice_log,linval = predglm)
library(rcompanion)
plot(trueval~linval,data = validtestdf_glm)
cor(validtestdf_glm)                                  # 0.8812831 // 0.9059177

## Residuals vs fitted Plot

par(mfrow=c(2,2))
plot(train1.glm)

## Prediction
predglm = predict(train1.glm, test_svm)
predgmodel = data.frame(Id = test$Id, SalePrice = predglm)

sapply(predgmodel,function(x) sum(is.na(x)))

# predgmodel$SalePrice[ is.na(predgmodel$SalePrice) |
#                        is.nan(predgmodel$SalePrice)] = median(predgmodel$SalePrice)

# predgmodel$SalePrice = predgmodel$SalePrice*10000

write.csv(predgmodel, "3rd Model_GLM.csv", row.names = FALSE)

###################################################################################################