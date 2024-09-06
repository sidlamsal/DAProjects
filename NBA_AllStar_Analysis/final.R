
#get initial data set #########################################################
raw <- read.csv("C:/Users/21sla/Downloads/nbaStats/22-23stats.csv")
#raw <- read.csv("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/nbaStats/22-23stats.csv")
raw["year"] <- 2023

#get other csv files and combine
years <- list(22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9)

for (i in 1:13){
  f <- paste("C:/Users/21sla/Downloads/nbaStats/", years[i+1], "-", years[i], "stats.csv", sep="")
  #f <- paste("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/nbaStats/", years[i+1], "-", years[i], "stats.csv", sep="")
  print(f)
  temp <- read.csv(f)
  temp["year"] <- (as.integer(years[i])+2000)
  raw <- rbind(raw, temp)
}

sum(is.na(raw))
#set row names
raw$Player <- paste(raw$Player, raw$year)
#removed duplicates (playes that were traded) bc wouldnt effect data too much
raw <- raw[!duplicated(raw$Player),]
row.names(raw) <- raw$Player
raw <- na.omit(raw)

#set response var
raw["AllStar"] <- 0

#get df of all stars
allStars <- read.csv("C:/Users/21sla/Downloads/AllStars/23as.csv")
#allStars <- read.csv("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/AllStars/23as.csv")
allStars["year"] <- 2023

for (i in 1:13){
  years[i]
  f <- paste("C:/Users/21sla/Downloads/AllStars/",years[i],"as.csv", sep="")
  #f <- paste("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/AllStars/",years[i],"as.csv", sep="")
  print(f)
  temp <- read.csv(f)
  temp["year"] <- (as.integer(years[i])+2000)
  allStars <- rbind(allStars, temp)
}

allStars$Starters <- paste(allStars$Starters, allStars$year)
row.names(allStars) <- allStars$Starters

#if player is an all star, set all star to 1
raw[row.names(allStars),]$AllStar <- 1
nrow(raw[raw$AllStar==1,])

#remove irrelevant vars
raw<- raw[,-c(1,2,3,4,5,31)]

#check and set var types
str(raw)
raw$AllStar <- as.factor(raw$AllStar)
str(raw)

#set test set to 2023 and train everything else
test<- na.omit(raw[raw$year==2023, -26])
train<- na.omit(raw[raw$year!=2023, -26])

#distribution of classes #######################################################
# Create data for the graph.
x <-  c(nrow(train[train$AllStar==1,]), nrow(train[train$AllStar==0,]))
labels <- c("All-Stars", "Not All-Stars")
piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = paste(piepercent, "%", sep=""), main = "All-Star Distribution Pie Chart",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8,
       fill = rainbow(length(x)))

distribution <- function(df){
  print(nrow(df[df$AllStar==1,]))
  print(nrow(df[df$AllStar==0,]))
}

# Create data for the graph.
x <-  c(nrow(test[test$AllStar==1,]), nrow(test[test$AllStar==0,]))
labels <- c("All-Stars", "Not All-Stars")
piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = paste(piepercent, "%", sep=""), main = "All-Star Distribution Pie Chart",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8,
       fill = rainbow(length(x)))

distribution <- function(df){
  print(nrow(df[df$AllStar==1,]))
  print(nrow(df[df$AllStar==0,]))
}


overSample <- train
distribution(overSample)

class1df <- overSample[overSample$AllStar==1,]
distribution(class1df)
class0df <- overSample[overSample$AllStar==0,]
distribution(class0df)

numNeededRows <- nrow(class0df)


overSampleIndex <- sample(1:nrow(class1df), numNeededRows, replace = TRUE)
length(overSampleIndex)

overSample <- rbind(class0df, class1df[overSampleIndex,])

distribution(overSample)

#Models ########################################################################
accuracy <- function(confMatrix){#Accuracy function
  return(sum(diag(confMatrix))/sum(confMatrix))
}
TPRate <- function(confMatrix){#Accuracy function
  return(confMatrix[4]/(confMatrix[2] + confMatrix[4]))
}

library(ROCR)
plotROC <- function(df){#ROC and AUC function
  pred <- prediction(df$predictions, df$test.AllStar)
  perf <- performance(pred,"tpr","fpr")
  plot(perf,colorize=TRUE, main="ROC Curve")
  
  auc_ROCR <- performance(pred, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  print(paste("AUC: ", auc_ROCR))
}
evaluateModel <- function(predictions, bestThresh){#print accuracy and plot func
  
  predictionClass <- ifelse(predictions>bestThresh, 1,0)
  
  conf <- table(test$AllStar, predictionClass)
  print(conf)
  print(paste("accuracy:", accuracy(conf)))
  print(paste("TP Rate: ", TPRate(conf)))
  
  df <- data.frame(predictions, test$AllStar)
  dfnona <- na.omit(df)
  
  plotROC(dfnona)
}
library(caret)
crossValidationFunction <- function(folds, train){#cross validate func
  n <- nrow(train)
  
  allsets <- createFolds(1:n, k=folds)
  
  validationErrors <- 0
  
  for (k in 1:folds){
    currentvalidation <- train[allsets[[k]],]
    currentraining <- setdiff(train, currentvalidation)
    
    currentlogModel <- glm(AllStar~., family = "binomial", data = currentraining)
    
    currentprediction <- predict(currentlogModel, currentvalidation, type = "response")
    bestT <- bestThreshold(currentprediction, currentvalidation)
    curentpredictionClass <- ifelse(currentprediction>bestT, 1,0)
    currentconfMatrix <- table(currentvalidation$AllStar, curentpredictionClass)
    currentaccuracy <- accuracy(currentconfMatrix)
    #print(currentaccuracy)
    validationErrors<- validationErrors + currentaccuracy
  }
  
  crossValidationAccuracy <- validationErrors/folds
  return(crossValidationAccuracy)
}

bestThreshold <- function(predictions, test){
  bestThresh <- 0
  bestAcc <- 0
  for (i in 1:100){
    predictionClass <- ifelse(predictions>(i/100), 1,0)
    conf <- table(test$AllStar, predictionClass)
    acc<- accuracy(conf)
    if (acc>bestAcc){
      bestAcc <- acc
      bestThresh <- (i/100)
    }
  }
  print(paste("best accuracy: ", bestAcc))
  print(paste("best threshold: ", bestThresh))
  return(bestThresh)
}
whoDidIMiss <- function(predictions, thresh){
  predictionClass <- ifelse(predictions>thresh, 1,0)
  predictedAS <- data.frame(predictionClass[predictionClass==1])
  predicted <- test
  predicted["model"] <- 0
  predicted[row.names(predictedAS),"model"] <- 1
  return (predicted[predicted[,"model"] != predicted[,"AllStar"],c("model", "AllStar")])
}

#log model
FullModel <- glm(AllStar~., data=train, family = "binomial")
#summary(FullModel)
fullPredictions <- predict(FullModel, test, type="response")
fullThresh <- bestThreshold(fullPredictions, test)
evaluateModel(fullPredictions, fullThresh)
whoDidIMiss(fullPredictions, fullThresh)
summary(FullModel)

#log model oversample
OversampleModel <- glm(AllStar~., data=overSample, family = "binomial")
overPredictions <- predict(OversampleModel, test, type="response")
overthresh <- bestThreshold(overPredictions, test)
evaluateModel(overPredictions, overthresh)
whoDidIMiss(overPredictions, overthresh)
summary(OversampleModel)
hist(c(overPredictions[overPredictions>.5], fullPredictions[fullPredictions>.5]), col=c("blue", "red"), 
     main="Histogram of Probabilities greater than 0.5", xlab="Probability")
legend("topleft", c("Full model", "Oversampled Model"), col=c("blue", "red"), lwd=1)

library(MASS)
cvLDA <- function(folds, train){#cross validate func
  n <- nrow(train)
  
  allsets <- createFolds(1:n, k=folds)
  
  validationErrors <- 0
  
  for (k in 1:folds){
    currentvalidation <- train[allsets[[k]],]
    currentraining <- setdiff(train, currentvalidation)
    
    currentlogModel <- lda(AllStar~., family = "binomial", data = currentraining)
    
    currentprediction <- predict(currentlogModel, currentvalidation, type = "response")
    currentprediction <- currentprediction$posterior[,2]
    bestT <- bestThreshold(currentprediction, currentvalidation)
    curentpredictionClass <- ifelse(currentprediction>bestT, 1,0)
    currentconfMatrix <- table(currentvalidation$AllStar, curentpredictionClass)
    currentaccuracy <- accuracy(currentconfMatrix)
    #print(currentaccuracy)
    validationErrors<- validationErrors + currentaccuracy
  }
  
  crossValidationAccuracy <- validationErrors/folds
  return(crossValidationAccuracy)
}
#LDA Model
LDAModel <- lda(AllStar~., data = train)
LDApredictions <- predict(LDAModel, test)
LDApredictions <- LDApredictions$posterior[,2]
ldathresh <- bestThreshold(LDApredictions, test)
evaluateModel(LDApredictions, ldathresh)
whoDidIMiss(LDApredictions, ldathresh)
cvLDA(5, train)


#LDA Model Oversample
LDAModel <- lda(AllStar~., data = overSample)
LDApredictions <- predict(LDAModel, test)
LDApredictions <- LDApredictions$posterior[,2]
ldathresh <- bestThreshold(LDApredictions, test)
evaluateModel(LDApredictions, ldathresh)
whoDidIMiss(LDApredictions, ldathresh)
cvLDA(5, overSample)

cvQDA <- function(folds, train){#cross validate func
  n <- nrow(train)
  
  allsets <- createFolds(1:n, k=folds)
  
  validationErrors <- 0
  
  for (k in 1:folds){
    currentvalidation <- train[allsets[[k]],]
    currentraining <- setdiff(train, currentvalidation)
    
    currentlogModel <- qda(AllStar~., family = "binomial", data = currentraining)
    
    currentprediction <- predict(currentlogModel, currentvalidation, type = "response")
    currentprediction <- currentprediction$posterior[,2]
    bestT <- bestThreshold(currentprediction, currentvalidation)
    curentpredictionClass <- ifelse(currentprediction>bestT, 1,0)
    currentconfMatrix <- table(currentvalidation$AllStar, curentpredictionClass)
    currentaccuracy <- accuracy(currentconfMatrix)
    #print(currentaccuracy)
    validationErrors<- validationErrors + currentaccuracy
  }
  
  crossValidationAccuracy <- validationErrors/folds
  return(crossValidationAccuracy)
}
#QDA Model
QDAModel <- qda(AllStar~., data = train)
QDApredictions <- predict(QDAModel, test)
QDApredictions <- QDApredictions$posterior[,2]
qdathresh <- bestThreshold(QDApredictions, test)
evaluateModel(QDApredictions, qdathresh)
cvQDA(5, train)

#QDA Model Oversample
QDAModel <- qda(AllStar~., data = overSample)
QDApredictions <- predict(QDAModel, test)
QDApredictions <- QDApredictions$posterior[,2]
qdathresh <- bestThreshold(QDApredictions, test)
evaluateModel(QDApredictions, qdathresh)
cvQDA(5, overSample)

#Variable selection ############################################################
library(leaps)
library(MASS)
library(glmnet)

forward <- stepAIC(FullModel, direction="forward", trace=FALSE)
forwardPredictions <- predict(forward, test, type="response")
forwardThresh <- bestThreshold(forwardPredictions, test)
evaluateModel(forwardPredictions, forwardThresh)
whoDidIMiss(forwardPredictions, forwardThresh)
summary(forward)

backwards <- stepAIC(FullModel, direction="backward", trace=FALSE)
backwardsPredictions <- predict(backwards, test, type="response")
backwardsThresh <- bestThreshold(backwardsPredictions, test)
evaluateModel(backwardsPredictions, backwardsThresh)
whoDidIMiss(backwardsPredictions, backwardsThresh)
summary(backwards)
backwards$formula


RSSModel <- regsubsets(AllStar~. + (PTS*eFG.), data=train, nvmax = 25)
RSSSummary <- summary(RSSModel)
names(RSSSummary)
RSSSummary$which[15,]

plot(RSSSummary$rsq, xlab = "Number of Variables", ylab = "rsq", type = "l")
plot(RSSSummary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "l")
plot(RSSSummary$cp, xlab = "Number of Variables", ylab = "cp", type = "l")
plot(RSSSummary$bic, xlab = "Number of Variables", ylab = "bic", type = "l")

n3Model <- glm(AllStar~GS+ MP+ FG.+ X2PA+ eFG.+ FT+ FT.+ DRB+ AST+ STL+ BLK+ TOV+ PF+ PTS+ (eFG.*PTS), data=train, family = "binomial")
summary(n3Model)
n3predict <- predict(n3Model, test, type="response")
n3thresh <- bestThreshold(n3predict, test)
evaluateModel(n3predict, n3thresh)
whoDidIMiss(n3predict, n3thresh)


x <- model.matrix(AllStar~.+(PTS*eFG.), train)
y <- train$AllStar
grid <- 10^seq(1, -2, length = 10)

#8
cvRidge <- cv.glmnet(x, y, alpha = 0, family="binomial")
optimalLambsaRidge <- cvRidge$lambda.min
ridgeOptimal <- glmnet(x, y, alpha = 0, lambda = optimalLambsaRidge, family = "binomial")

#9
cvLasso <- cv.glmnet(x, y, alpha = 1, family="binomial")
optimalLambdaLasso <- cvLasso$lambda.min
LassoOptimal <- glmnet(x, y, alpha = 1, lambda = 0.01, family = "binomial")

#Q8 ridgeOptimal
x2 <- model.matrix(AllStar~.+(PTS*eFG.), test)
predictionsRidge <- predict(ridgeOptimal, x2, type="response")
df <- data.frame(predictionsRidge, test$AllStar)
C <- na.omit(df)
colnames(C) <- c("predictions","test.AllStar")
plotROC(C)
ridgeThresh <- bestThreshold(C$predictions, test)
evaluateModel(C$predictions, ridgeThresh)
ridgeOptimal$beta

predicted <- test
predicted["model"] <- ifelse(C$predictions>ridgeThresh, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar")]









#Q8 LassoOptimal
predictionsLasso <- predict(LassoOptimal, x2, type="response")
df <- data.frame(predictionsLasso, test$AllStar)
D <- na.omit(df)
colnames(D) <- c("predictions","test.AllStar")
plotROC(D)
lassoThresh <- bestThreshold(D$predictions, test)
evaluateModel(D$predictions, lassoThresh)
coef(LassoOptimal)

correlations <- data.frame(cor(train[,-26]))
#plot(train[,c(4, 5, 7, 8, 10, 11, 14, 15, 18, 19, 25)])
##############################################################################

#log model
FullModel <- glm(AllStar~. + (PTS*eFG.) + .^2, data=train, family = "binomial")
summary(FullModel)
fullPredictions <- predict(FullModel, test, type="response")
fullThresh <- bestThreshold(fullPredictions, test)
evaluateModel(fullPredictions, fullThresh)
whoDidIMiss(fullPredictions, fullThresh)
summary(FullModel)




crossValidationFunction(5, train)
crossValidationFunction(5, overSample)

################################################################################

library("tree")
#Add eval tree method
whoDidIMissTree <- function(predictions){
  predictedAS <- data.frame(predictions[predictions==1])
  predicted <- test
  predicted["model"] <- 0
  predicted[row.names(predictedAS),"model"] <- 1
  return (predicted[predicted[,"model"] != predicted[,"AllStar"],c("model", "AllStar")])
}
treeModel <- tree(AllStar~., train, split='gini')
#treeModel <- tree(AllStar~., train)
summary(treeModel)
treeModel

treeOverModel <- tree(AllStar~., overSample)
summary(treeOverModel)

plot(treeModel)
text(treeModel, pretty = 0)

plot(treeOverModel)
text(treeOverModel, pretty = 0)

treePred <- predict(treeModel, test, type = "class")
treeOverPred <- predict(treeOverModel, test, type = "class")

table(test$AllStar, treePred)
table(test$AllStar, treeOverPred)

accuracy(table(train$AllStar, treePred))
accuracy(table(test$AllStar, treeOverPred))

TPRate(table(test$AllStar, treePred))
TPRate(table(test$AllStar, treeOverPred))


#Bagging
library(randomForest)
bagTree <- randomForest(AllStar~., data=train, mtry=25)
bagTree

bagPredict <- predict(bagTree, test)
table(test$AllStar, bagPredict)
accuracy(table(test$AllStar, bagPredict))
TPRate(table(test$AllStar, bagPredict))

importance(bagTree)
varImpPlot(bagTree)

bagTreeOver <- randomForest(AllStar~., data=overSample, mtry=25)
bagTreeOver

bagPredictOver <- predict(bagTreeOver, test)
table(test$AllStar, bagPredictOver)

accuracy(table(test$AllStar, bagPredictOver))
TPRate(table(test$AllStar, bagPredictOver))

importance(bagTreeOver)
varImpPlot(bagTreeOver)

#Random Forrest
randomTree <- randomForest(AllStar~., data=train, mtry=5)
randomTree

randomPredict <- predict(randomTree, test)
table(test$AllStar, randomPredict)
accuracy(table(test$AllStar, randomPredict))
TPRate(table(test$AllStar, randomPredict))

importance(randomTree)
varImpPlot(randomTree)

randomTreeOver <- randomForest(AllStar~., data=overSample, mtry=5)
randomTreeOver

randomPredictOver <- predict(randomTreeOver, test)
table(test$AllStar, randomPredictOver)
accuracy(table(test$AllStar, randomPredictOver))
TPRate(table(test$AllStar, randomPredictOver))

importance(randomTreeOver)
varImpPlot(randomTreeOver)

whoDidIMissTree(randomPredict)

#Boosting
#install.packages("gbm")
#install.packages("xgboost")
#install.packages("caret")
library(caret)
library(xgboost)
library(gbm)

boostedTree <- train(AllStar~., data = train, method = "gbm", trControl = trainControl("cv", number = 10))
summary(boostedTree)


boostedPredict <- predict(boostedTree, test)
table(test$AllStar, boostedPredict)

predicted <- test
predicted["model"] <- ifelse(boostedPredict==1, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar")]

accuracy(table(train$AllStar, boostedPredict))
TPRate(table(train$AllStar, boostedPredict))


prunedTree <- cv.tree(treeOverModel, FUN = prune.misclass)
names(prunedTree)
prunedTree$size
prunedTree$dev

plot(prunedTree$size, prunedTree$dev, type = "b")
plot(prunedTree$k, prunedTree$dev, type = "b")

optimalprunedTree <- prune.misclass(treeModel, best = 20)
plot(optimalprunedTree)
text(optimalprunedTree, pretty = 0)

optimalPred <- predict(optimalprunedTree, test, type = "class")

table(test$AllStar, optimalPred)
###############################################################################
#SVM
#install.packages("e1071")
library(e1071)

dat <- data.frame(x = train[,-26], y = as.factor(train$AllStar))
#dat <- data.frame(x = overSample[,-26], y = as.factor(overSample$AllStar))

tune.out.poly <- tune(svm, y~., data = dat, kernel = "polynomial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune.out.line <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune.out.rad <- tune(svm, y~., data = dat, kernel = "radial",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out.poly)
summary(tune.out.line)
summary(tune.out.rad)

bestPoly <- tune.out.poly$best.model
bestLine <- tune.out.line$best.model
bestRad <- tune.out.rad$best.model

testdat <- data.frame(x = test[,-26], y = as.factor(test$AllStar))

ypredPoly <- predict(bestPoly, testdat)
ployMat <- table(truth = testdat$y, predict = ypredPoly)
predicted <- test
predicted["model"] <- ifelse(ypredPoly==1, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar")]
accuracy(ployMat)
TPRate(ployMat)

ypredLine <- predict(bestLine, testdat)
lineMat <- table(truth = testdat$y, predict = ypredLine)
accuracy(lineMat)
TPRate(lineMat)

ypredRad <- predict(bestRad, testdat)
radMat <- table(truth = testdat$y, predict = ypredRad)
accuracy(radMat)
TPRate(radMat)

predicted <- test
predicted["model"] <- ifelse(boostedPredict==1, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar")]




###############################################################################
#Advanced stats
#get initial data set #########################################################
#raw2 <- read.csv("C:/Users/21sla/Downloads/nbaAdvancedStats/22-23stats.csv")
raw2 <- read.csv("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/nbaAdvancedStats/22-23stats.csv")
raw2["year"] <- 2023

#get other csv files and combine
years <- list(22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9)

for (i in 1:13){
  #f <- paste("C:/Users/21sla/Downloads/nbaAdvancedStats/", years[i+1], "-", years[i], "stats.csv", sep="")
  f <- paste("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/nbaAdvancedStats/", years[i+1], "-", years[i], "stats.csv", sep="")
  print(f)
  temp <- read.csv(f)
  temp["year"] <- (as.integer(years[i])+2000)
  raw2 <- rbind(raw2, temp)
}

raw2<- raw2[,-c(20, 25)]

sum(is.na(raw2))
#set row names
raw2$Player <- paste(raw2$Player, raw2$year)
#removed duplicates (playes that were traded) bc wouldnt effect data too much
raw2 <- raw2[!duplicated(raw2$Player),]
row.names(raw2) <- raw2$Player
raw2 <- na.omit(raw2)

#set response var
raw2["AllStar"] <- 0

#get df of all stars
#allStars <- read.csv("C:/Users/21sla/Downloads/AllStars/23as.csv")
allStars <- read.csv("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/AllStars/23as.csv")
allStars["year"] <- 2023

for (i in 1:13){
  years[i]
  #f <- paste("C:/Users/21sla/Downloads/AllStars/",years[i],"as.csv", sep="")
  f <- paste("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data300/AllStars/",years[i],"as.csv", sep="")
  print(f)
  temp <- read.csv(f)
  temp["year"] <- (as.integer(years[i])+2000)
  allStars <- rbind(allStars, temp)
}

allStars$Starters <- paste(allStars$Starters, allStars$year)
row.names(allStars) <- allStars$Starters

#if player is an all star, set all star to 1
raw2[row.names(allStars),]$AllStar <- 1
nrow(raw2[raw2$AllStar==1,])

raw2 <- raw2[,-c(1, 2, 3, 4, 5, 28)]

#check and set var types
str(raw2)
raw2$AllStar <- as.factor(raw2$AllStar)
str(raw2)

raw3<- merge(raw, raw2,
             by = 'row.names', all = TRUE)

#removed duplicates (playes that were traded) bc wouldnt effect data too much
row.names(raw3) <- raw3$Row.names
raw3 <- na.omit(raw3)

raw3 <- raw3[, -1]
#raw3 <- raw3[raw3$G.x>20, -1]

test<- na.omit(raw3[raw3$year.y==2023, -c(1, 3, 26, 27, 50)])
train<- na.omit(raw3[raw3$year.y!=2023, -c(1, 3, 26, 27, 50)])

#log model
FullModel <- glm(AllStar.y~., data=train, family = "binomial")
#summary(FullModel)
fullPredictions <- predict(FullModel, test, type="response")
fullThresh <- bestThreshold(fullPredictions, test)
evaluateModel(fullPredictions, fullThresh)
whoDidIMiss(fullPredictions, fullThresh)
summary(FullModel)

predicted <- test
predicted["model"] <- ifelse(fullPredictions>fullThresh, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar.y")]


x <- model.matrix(AllStar.y~., train)
y <- train$AllStar.y
grid <- 10^seq(1, -2, length = 10)

#8
cvRidge <- cv.glmnet(x, y, alpha = 1, family="binomial")
optimalLambsaRidge <- cvRidge$lambda.min
ridgeOptimal <- glmnet(x, y, alpha = 1, lambda = .01, family = "binomial")
sort(ridgeOptimal$beta)

#Q8 ridgeOptimal
x2 <- model.matrix(AllStar.y~., test)
predictionsRidge <- predict(ridgeOptimal, x2, type="response")
df <- data.frame(predictionsRidge, test$AllStar.y)
C <- na.omit(df)
colnames(C) <- c("predictions","test.AllStar.y")
plotROC(C)
ridgeThresh <- bestThreshold(C$predictions, test)
evaluateModel(C$predictions, ridgeThresh)

predicted <- test
predicted["model"] <- ifelse(C$predictions>ridgeThresh, 1,0)
predicted[predicted$model != predicted$AllStar,c("model", "AllStar.y")]

forward <- stepAIC(FullModel, direction="forward", trace=FALSE)
forwardPredictions <- predict(forward, test, type="response")
forwardThresh <- bestThreshold(forwardPredictions, test)
evaluateModel(forwardPredictions, forwardThresh)
whoDidIMiss(forwardPredictions, forwardThresh)
summary(forward)

backwards <- stepAIC(FullModel, direction="backward", trace=FALSE)
backwardsPredictions <- predict(backwards, test, type="response")
backwardsThresh <- bestThreshold(backwardsPredictions, test)
evaluateModel(backwardsPredictions, backwardsThresh)
whoDidIMiss(backwardsPredictions, backwardsThresh)
summary(backwards)
backwards$formula


Player <- as.factor(c("Devin Booker", 
                      "Jalen Brunson", 
                      "Jimmy Butler", 
                      "Anthony Davis", 
                      "James Harden", 
                      "Jrue Holiday", 
                     "Brandon Ingram", 
                      "Jaren Jackson Jr.", 
                      "Zach LaVine", 
                     "Kawhi Leonard", 
                      "Kristaps Porziņģis", 
                      "Trae Young",
                     "Bam Adebayo", 
                     "Devin Booker", 
                     "Jimmy Butler",
                     "Anthony Davis", 
                     "Paul George", 
                     "James Harden", 
                     "Jrue Holiday", 
                     "Jaren Jackson Jr.", 
                     "Trae Young", 
                     "Devin Booker", 
                     "Jimmy Butler", 
                     "Anthony Davis", 
                     "Darius Garland", 
                     "Jalen Green", 
                     "James Harden", 
                     "Jrue Holiday", 
                     "Jaren Jackson Jr.", 
                     "Keldon Johnson", 
                     "Zach LaVine", 
                     "Kawhi Leonard", 
                     "Kristaps Porziņģis", 
                     "Zion Williamson", 
                     "Trae Young",
                     "Bam Adebayo", 
                     "Devin Booker", 
                     "Jalen Brunson", 
                     "Jimmy Butler", 
                     "Anthony Davis", 
                     "Paul George", 
                     "James Harden", 
                     "Jrue Holiday", 
                     "Jaren Jackson Jr.", 
                     "Zach LaVine", 
                     "Kawhi Leonard", 
                     "Kristaps Porziņģis", 
                     "Trae Young" 
                     ))
freqtable <- data.frame(sort(table(Player)))


barplot(sort(table(names)), main="Incorrect Prediction Frequency",
        ylab="Count", las = 2, col=4)



ggplot(freqtable, aes(Player, Freq)) +    # ggplot2 plot with modified x-axis labels
  geom_bar(stat = "identity",fill="darkblue") +
  theme(axis.text.x = element_text(angle = 90, size = 12)) +
  ggtitle("Misclassification Frequency")






