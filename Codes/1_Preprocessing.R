# Predictive Analytics on Ames housing data (Housing Data -Kaggle)
# By - Chetan Jawale.


## Data Loading

ames_train <- read.csv(file.choose()) 
ames_test <- read.csv(file.choose())
View(ames_train)

## For Some Variables NA represents as perticular feature is'NOT Present'
## Hence, such observations should not be considered as missing values
## Releveling NA to No

newNA_train <- ames_train[,c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")]
str(newNA_train)

library(forcats) 
ames_train$Alley <- fct_explicit_na(ames_train$Alley, na_level = "No")
ames_train$BsmtQual <- fct_explicit_na(ames_train$BsmtQual, na_level = "No")
ames_train$BsmtCond <- fct_explicit_na(ames_train$BsmtCond, na_level = "No")
ames_train$BsmtExposure <- fct_explicit_na(ames_train$BsmtExposure, na_level = "No")
ames_train$BsmtFinType1 <- fct_explicit_na(ames_train$BsmtFinType1, na_level = "No")
ames_train$BsmtFinType2 <- fct_explicit_na(ames_train$BsmtFinType2, na_level = "No")
ames_train$FireplaceQu <- fct_explicit_na(ames_train$FireplaceQu, na_level = "No")
ames_train$GarageType <- fct_explicit_na(ames_train$GarageType, na_level = "No")
ames_train$GarageFinish <- fct_explicit_na(ames_train$GarageFinish, na_level = "No")
ames_train$GarageQual <- fct_explicit_na(ames_train$GarageQual, na_level = "No")
ames_train$GarageCond <- fct_explicit_na(ames_train$GarageCond, na_level = "No")
ames_train$PoolQC <- fct_explicit_na(ames_train$PoolQC, na_level = "No")
ames_train$MiscFeature <- fct_explicit_na(ames_train$MiscFeature, na_level = "No")
ames_train$Fence <- fct_explicit_na(ames_train$Fence, na_level = "No")

newNA_train <- ames_train[,c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")]
str(newNA_train)
levels(ames_train$GarageType)

## Checking for missing values

sapply(ames_train,function(x) sum(is.na(x)))

## Deleting variables having more than 40% missing data

ames_train[, -which(colMeans(is.na(ames_train)) > 0.4)]
# ames_train <- ames_train[, -which(colMeans(is.na(ames_train)) > 0.4)]
dim(ames_train)

## Rechecking for missing values

sapply(ames_train,function(x) sum(is.na(x)))

## Filling missing values in categorical variables and continuous variables by mode
#   and mean respectively.

# Creating function called Mode

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- NA
  return(xmode)
}

# Making use of for loop to substitute missing values at once

for (var in 1:ncol(ames_train)) {
  if (class(ames_train[,var])=="integer") {
    ames_train[is.na(ames_train[,var]),var] <- mean(ames_train[,var], na.rm = TRUE)
  } else if (class(ames_train[,var]) %in% c("factor")) {
    ames_train[is.na(ames_train[,var]),var] <- Mode(ames_train[,var], na.rm = TRUE)
  }
}

# Rechecking missing values

sapply(ames_train,function(x) sum(is.na(x)))


## Exporting the clean training dataset to excel

write.csv(ames_train, file = "ames_train_cleaned.csv", row.names = FALSE)

#########################################################################################
## Repeating the same steps on test dataset

newNA_test <- ames_test[,c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")]
str(newNA_test)

library(forcats) 
ames_test$Alley <- fct_explicit_na(ames_test$Alley, na_level = "No")
ames_test$BsmtQual <- fct_explicit_na(ames_test$BsmtQual, na_level = "No")
ames_test$BsmtCond <- fct_explicit_na(ames_test$BsmtCond, na_level = "No")
ames_test$BsmtExposure <- fct_explicit_na(ames_test$BsmtExposure, na_level = "No")
ames_test$BsmtFinType1 <- fct_explicit_na(ames_test$BsmtFinType1, na_level = "No")
ames_test$BsmtFinType2 <- fct_explicit_na(ames_test$BsmtFinType2, na_level = "No")
ames_test$FireplaceQu <- fct_explicit_na(ames_test$FireplaceQu, na_level = "No")
ames_test$GarageType <- fct_explicit_na(ames_test$GarageType, na_level = "No")
ames_test$GarageFinish <- fct_explicit_na(ames_test$GarageFinish, na_level = "No")
ames_test$GarageQual <- fct_explicit_na(ames_test$GarageQual, na_level = "No")
ames_test$GarageCond <- fct_explicit_na(ames_test$GarageCond, na_level = "No")
ames_test$PoolQC <- fct_explicit_na(ames_test$PoolQC, na_level = "No")
ames_test$MiscFeature <- fct_explicit_na(ames_test$MiscFeature, na_level = "No")
ames_test$Fence <- fct_explicit_na(ames_test$Fence, na_level = "No")

newNA_test <- ames_test[,c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC","Fence","MiscFeature")]
str(newNA_test)
levels(ames_test$GarageType)

## Checking for missing values

sapply(ames_test,function(x) sum(is.na(x)))

## Deleting variables having more than 40% missing data

ames_test[, -which(colMeans(is.na(ames_test)) > 0.4)]
# ames_test <- ames_test[, -which(colMeans(is.na(ames_test)) > 0.4)]
dim(ames_test)

## Rechecking for missing values

sapply(ames_test,function(x) sum(is.na(x)))

## Filling missing values in categorical variables and continuous variables by mode
#   and mean respectively.

# Making use of for loop to substitute missing values at once

for (var in 1:ncol(ames_test)) {
  if (class(ames_test[,var])=="integer") {
    ames_test[is.na(ames_test[,var]),var] <- mean(ames_test[,var], na.rm = TRUE)
  } else if (class(ames_test[,var]) %in% c("factor")) {
    ames_test[is.na(ames_test[,var]),var] <- Mode(ames_test[,var], na.rm = TRUE)
  }
}

# Rechecking missing values

sapply(ames_test,function(x) sum(is.na(x)))

## Exporting the clean testing dataset to excel

write.csv(ames_test, file = "ames_test_cleaned.csv", row.names = FALSE)