#Loading relevant libraries

library(caret)
library(ggplot2)
library(corrplot)
library(lattice)
library(kernlab)
library(rattle)

#Set working directory

setwd("C:/Users/dimut/desktop/Data Science/Practical Machine Learning")

#Load data sets

testing <- read.csv("pml-testing.csv")
training <- read.csv("pml-training.csv")

#Exploring data

dim(training)
dim(testing)
typeof(training)

#Cleaning data
#Removing the missing values

trainData<- training[, colSums(is.na(training)) == 0]
testData <- testing[, colSums(is.na(testing)) == 0]
dim(trainData)
dim(testData)

#Remove unnecessary varaiables

trainData <- trainData[,-c(1:7)]
testData <- testData[,-c(1:7)]
dim(trainData)
dim(testData)

#Removing zero covariates

nvz <- nearZeroVar(trainData)
trainData <- trainData[,-nvz]
dim(trainData)
typeof(trainData)

#Creating a correlation plot of the variables

D <- trainData
D <- as.data.frame(unclass(D))#converting list into factor
corrPlot <- cor(trainData[, -length(names(trainData))])
corrplot(corrPlot, method="circle",diag = FALSE, order = "hclust",type = "upper")

#Splitting train data into train and validation sets

set.seed(100) 
intrain <- createDataPartition(trainData$classe, p = 0.8, list = FALSE)
trainData <- trainData[intrain, ]
validationData <- trainData[-intrain, ]
dim(trainData)
dim(validationData)

#cross validation
cv <- trainControl(method="cv", number=5, verboseIter=F)

#Decision tree
set.seed(125)
modDT <- train(classe ~ ., method = "rpart", data = trainData, trControl = cv)
fancyRpartPlot(modDT$finalModel)

#Prediction
DT <- predict(modDT, validationData)
DT_results <- confusionMatrix(DT, factor(validationData$classe))
DT_results

#Random forest
set.seed(125)
modRF <- train(classe~., data=trainData, method="rf", trControl = cv)
RF <- predict(modRF, validationData)
RF_Results<- confusionMatrix(RF, factor(validationData$classe))
RF_Results

#Gradient boosted trees
set.seed(125)
modBT <- train(classe~., data=trainData, method="gbm", trControl = cv, verbose = F)
BT <- predict(modBT, validationData)
BT_results <- confusionMatrix(BT, factor(validationData$classe))
BT_results

#RF with test data

RF1 <- predict(modRF, testData)
RF1

















