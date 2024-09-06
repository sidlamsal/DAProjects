
library(MASS)
library(dplyr)
library(tibble)
library(ggplot2)
library(class)


data <- read.csv("/Users/sidlamsal/Library/CloudStorage/OneDrive-DickinsonCollege/Data180/masterTable.csv")
data <- read.csv("C:/Users/21sla/OneDrive - Dickinson College/Data180/masterTable.csv")
#Data between 1980 and 2021 (considered the modern era)
ncol(data)

#Cleaning the data set

data <- data %>%
  mutate(isMVP = ifelse(Rank == 1, 1, 0))%>%
  rename(MVP = Rank)
  
data$MVP = data$isMVP
data$MVP <- as.factor(data$MVP)

scaledPoints <- data$Share*1000
data$Pts.Won <- scaledPoints
data <- rename(data, scaledPtsWon = Pts.Won)

data <- data[-36,-c(5,7,8,22, 40, 41, 46)] 

player <- paste(data$Player, data$year, sep = ' ')
data$Player = player
data<- remove_rownames(data)
data<- column_to_rownames(data, "Player")

data<- data[,-17]
data<- data[,-3]



mvpData <- data[which(data$MVP==1),]
realCans <- data[which(data$scaledPtsWon>=100),]

ncol(data)
#Visualization 

# #bar plot of age of MVP
# freqTable = table(data$Age)
# freqTableMVP = table(mvpData$Age)
# barplot(freqTable, main = "Age of MVP Candidates", xlab = "Age", ylab = "Number of Candidates", col = 3, las = 2,)
# barplot(freqTableMVP, main = "Age of MVPs", xlab = "Age", ylab = "Number of MVPs", col = 3, las = 2)
# # bar graph of age for mvps
# ggplot(data, aes(x=Age, fill=MVP))+
#   geom_bar(position = "stack")+
#   labs(title = "MVP Candidate Age", x= "Age", y= "Players")
# 
# #Histograms for box plus minus and value over replacement 
# hist(data$box_plus_minus, main = "Box Plus Minus by MVP Candidates Histogram", xlab = "BPM Range", col = 4)
# hist(mvpData$box_plus_minus, main = "Box Plus Minus by MVPs Histogram", xlab = "BPM Range", col = 4)
# 
# hist(data$value_over_replacement_player, main = "Value Over Replacement Player by MVP Candidates Histogram", xlab = "VOR Range", col = 4)
# hist(mvpData$value_over_replacement_player, main = "Value Over Replacement by MVPs Histogram", xlab = "VORP Range", col = 4)
# 
# # bar graph of seeding for mvps
# ggplot(data, aes(x=seed, fill = MVP)) +
#   geom_bar(position = "stack")+
#   labs(title = "MVP Candidates By Seeding", x= "Seed", y= "Candidates")
# 
# #Scatter plot of Win shares and wins
# plot(data$W, data$WS, main = "MVP Candidate Win Shares and Wins", xlab = "Number of Wins", ylab = "Win Shares")
# points(mvpData$W,mvpData$WS, col = 5)  
# legend(x = 20, y = 20, c("Candidates","MVPs"),cex=.8,col=c("black","cyan"),pch=c(1,1))
# 
# 
# # box plot of PER
# boxplot(data$player_efficiency_rating, col = "white", main = "Player Efficiency Rating Boxplot", ylab = "PER")
# 
# stripchart(mvpData$player_efficiency_rating, method = "jitter", pch = 19, col = 4,vertical = TRUE,add = TRUE)        
# legend(x = "topright", c("MVPs"),col=4, pch = 19)

#Clustering 

# withinDist <- 0
# clusterList <- 1:10
# 
# for (i in clusterList){
#   MVPCluster <- kmeans(data[,-1], i)
#   withinDist <- c(withinDist, MVPCluster$tot.withinss)
# }
# plot(withinDist[-1], main = "Within-Group Distance vs Number of Groups", xlab = "Number of Groups", ylab = "Within-group Distance")
# 
# MVPCanKMeans <- kmeans(data[,-1], 4)
# 
# summary(MVPCanKMeans)
# MVPCanKMeans$centers
# 
# for (i in 1:4){
#   if (i ==1){
#     plot(data[which(MVPCanKMeans$cluster==i), c("W", "WS")], col = i,
#          ylim = c(min(data$WS), max(data$WS)),
#          xlim = c(min(data$W), max(data$W)),
#          main = "Clustered Win Shares And Wins")
#   }
#   else{
#     points(data[which(MVPCanKMeans$cluster==i), c("W", "WS")], col = i)
#   }
# }
# 
# for (i in 1:10){
#   if (i ==1){
#     plot(data[which(MVPCanKMeans$cluster==i), c("scaledPtsWon", "player_efficiency_rating")], col = i,
#          xlim = c(min(data$scaledPtsWon), max(data$scaledPtsWon)),
#          ylim = c(min(data$player_efficiency_rating), max(data$player_efficiency_rating)))
#   }
#   else{
#     points(data[which(MVPCanKMeans$cluster==i), c("scaledPtsWon", "player_efficiency_rating")], col = i)
#   }
# }
# 
# nrow(data[which(MVPCanKMeans$cluster == 3),])
# data[which(MVPCanKMeans$cluster == 4),3]
# 
# MVPDist<- dist(mvpData[,-c(3,4)], method = "euclidean")
# HCMVP <- hclust(MVPDist)
# plot(HCMVP)
# 
# clustered <- rect.hclust(HCMVP, k = 5, border = 1:5)


#Regression

#pairs(data[2:12])
#pairs(data[12:24])
#pairs(data[24:36])
#pairs(data[,-c(1,2, 4:16)])
#pairs(data[,c("scaledPtsWon", "G", "MP", "PTS", "WS", "WS.48", "W", "W.L.", "seed"
             #, "player_efficiency_rating", "true_shooting_percentage", 
             #"offensive_win_shares", "offensive_box_plus_minus", "box_plus_minus", 
             #"value_over_replacement_player", )])

#lmDataUserPicked<- data[,c("scaledPtsWon", "G", "MP", "PTS", "WS", "WS.48", "W", "W.L.", "seed"
                           #, "player_efficiency_rating", "true_shooting_percentage", 
                           #"offensive_win_shares", "offensive_box_plus_minus", "box_plus_minus", 
                           #"value_over_replacement_player")]

training <- sample(1:nrow(data), 0.8*nrow(data))
trainingSet <- data[training,]



validation <- setdiff(1:nrow(data), training)
validationSet <- data[validation,]


lmPointsKMS <- lm(scaledPtsWon~(Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+WS+WS.48+W+W.L.+seed+
                                  player_efficiency_rating+true_shooting_percentage+
                                  three_point_attempt_rate+free_throw_attempt_rate+
                                  offensive_rebound_percentage+defensive_rebound_percentage+
                                  total_rebound_percentage+assist_percentage+steal_percentage+
                                  block_percentage+turnover_percentage+usage_percentage+
                                  offensive_win_shares+ defensive_win_shares + offensive_box_plus_minus+
                                  defensive_box_plus_minus+box_plus_minus+value_over_replacement_player
                                  )^2, trainingSet[,-c(1)])


 round(summary(lmPointsKMS)$coef[which(summary(lmPointsKMS)$coef[, 4] <= 0.01), ], 4)
 summary(lmPointsKMS)$adj.r.squared
 summary(lmPointsKMS)$r.squared
# summary(lmPointsKMS)

lmPoints <- lm(scaledPtsWon~.+(seed*WS)+I(player_efficiency_rating^2) +(W*WS) +(turnover_percentage*player_efficiency_rating), trainingSet[,-1])
summ <- summary(lmPoints)
summ$r.squared
summ

#lmPointss <- lm(scaledPtsWon~.,data[,-1])
#summary(lmPointss)
#dim(model.matrix(lmPointsKMS))[2]
#modd <- model.matrix(lmPointsKMS)

#lmPointsInteraction <- lm(scaledPtsWon~.+I(player_efficiency_rating^2)+(W*W.L.)+(WS*WS.48), lmDataUserPicked)
#summary(lmPointsInteraction)

#revisedLmDataUserPicked<- data[,c("scaledPtsWon", "G", "WS", "WS.48", "W", "W.L.", "seed"
                           #, "player_efficiency_rating", 
                           #"offensive_win_shares", "offensive_box_plus_minus", "box_plus_minus")]

#lmPointsQuadratic <- lm(scaledPtsWon~.+I(player_efficiency_rating^2) + I(box_plus_minus^2)+(W*W.L.)+(WS*WS.48), revisedLmDataUserPicked)
#summary(lmPointsQuadratic)



# emptyLM <- lm(scaledPtsWon~1, trainingSet[,-1])
# fullLM <- lm(scaledPtsWon~., trainingSet[,-1])
# 
# var_sel <- stepAIC(emptyLM, direction = "forward", scope = list(upper = lmPoints, lower = emptyLM))
# summary(var_sel)
# 
# # KMSSelect <- stepAIC(emptyLM, direction = "forward", scope = list(upper = lmPointsKMS, lower = emptyLM),trace=0)
# # summary(KMSSelect)
# 
# fullSelect <- stepAIC(emptyLM, direction = "forward", scope = list(upper = lmPointss, lower = emptyLM),trace=0)
# summary(fullSelect)

# error = 0
# for (i in 1:100){
#   training <- sample(1:nrow(data), 0.8*nrow(data))
#   trainingSet <- data[training,]
# 
#   validation <- setdiff(1:nrow(data), training)
#   validationSet <- data[validation,]
# 
#   lmPointsKMS <- lm(scaledPtsWon~(Age+G+MP+PTS+TRB+AST+STL+BLK+FG.+X3P.+FT.+WS+WS.48+W+W.L.+seed+
#                                     player_efficiency_rating+true_shooting_percentage+
#                                     three_point_attempt_rate+free_throw_attempt_rate+
#                                     offensive_rebound_percentage+defensive_rebound_percentage+
#                                     total_rebound_percentage+assist_percentage+steal_percentage+
#                                     block_percentage+turnover_percentage+usage_percentage+
#                                     offensive_win_shares+ defensive_win_shares + offensive_box_plus_minus+
#                                     defensive_box_plus_minus+box_plus_minus+value_over_replacement_player
#   )^2, trainingSet[,-c(1)])
# 
#   KMSSelect <- stepAIC(emptyLM, direction = "forward", scope = list(upper = lmPointsKMS, lower = emptyLM),trace=0)
# 
#   predictionFull <- predict(KMSSelect, validationSet)
# 
#   thisMin = mean(abs(predictionFull-validationSet$scaledPtsWon), na.rm = TRUE)
# 
#   error = error + thisMin
# 
# }
# error/100


cans <- read.csv("C:/Users/21sla/OneDrive - Dickinson College/Data180/csvbook.csv")
#predictionKMS <- predict(lmPointsKMS, validationSet)
cans<- column_to_rownames(cans, "Player")


#predictFull <- predict(lmPointss, validationSet)
#predictionFullSelect <- predict(fullSelect, validationSet)

lmPoints <- lm(scaledPtsWon~.+(seed*WS)+I(player_efficiency_rating^2) +(W*WS) +(turnover_percentage*player_efficiency_rating), trainingSet[,-1])
lmPoints <- lm(scaledPtsWon~.+(seed*WS)+I(player_efficiency_rating^2) +(W*WS) +(turnover_percentage*player_efficiency_rating), data[,-1])

lmPoints <- lm(scaledPtsWon~.+I(player_efficiency_rating^2) +(turnover_percentage*player_efficiency_rating) + (WS*seed) + (box_plus_minus*W.L.), data[,-1])


lmPoints <- lm(scaledPtsWon~.+I(player_efficiency_rating^2) +(turnover_percentage*player_efficiency_rating) + (WS*seed) + (box_plus_minus*W.L.), data[-619,-1])

summary(lmPoints)
predictionLM <- predict(lmPoints, validationSet)
predictionLM <- predict(lmPoints, cans)

predictionLM <- predict(lmPoints, data["Russell Westbrook 2017",])



predictionLM




data["Russell Westbrook 2018",]
#predictionLMSelect <- predict(var_sel, validationSet)

#summary(var_sel_back)$adj.r.squared
#summary(var_sel_back)$r.squared


#mean(abs(predictFull-validationSet$scaledPtsWon), na.rm = TRUE)
#mean(abs(predictionFullSelect-validationSet$scaledPtsWon), na.rm = TRUE)
mean(abs(predictionLM-validationSet$scaledPtsWon), na.rm = TRUE)
#mean(abs(predictionLMSelect-validationSet$scaledPtsWon), na.rm = TRUE)


error = 0

#data<- data[,-c(2,6:13,21:28,31:34)]

for (i in 1:250){
  training <- sample(1:nrow(data), 0.8*nrow(data))
  trainingSet <- data[training,]

  validation <- setdiff(1:nrow(data), training)
  validationSet <- data[validation,]

  lmPoints <- lm(scaledPtsWon~.+I(player_efficiency_rating^2) +(turnover_percentage*player_efficiency_rating) + (WS*seed) + (box_plus_minus*W.L.)+(W*WS), trainingSet[,-1])

  predictionFull <- predict(lmPoints, validationSet)

  thisMin = mean(abs(predictionFull-validationSet$scaledPtsWon), na.rm = TRUE)
  
  #thisMin = mean((predictionFull-validationSet$scaledPtsWon), na.rm = TRUE)

  error = error + thisMin

}
summary(lmPoints)
error/250


#pairs(data[,-1])




























#Classification

deservesMVP <- data[]

deservesMVP$MVP <- ifelse(deservesMVP$scaledPtsWon>=658,1,0)



training <- sample(1:nrow(deservesMVP), 0.8*nrow(deservesMVP))
trainingSet <- data[training,-3]

# oversample_training <- sample(which(trainingSet$MVP == 1), 0.8*length(which(trainingSet$MVP == 0)),
#                                 replace = T)
# trainingSet <- rbind(trainingSet[which(trainingSet$MVP == 0), ],
#                        trainingSet[oversample_training, ])


validation <- setdiff(1:nrow(deservesMVP), training)
validationSet <- deservesMVP[validation,-3]




# K_list <- 1:20
# Acc<- 0
# 
# for (k in K_list){
#   knn_pred <- knn(trainingSet,
#                   validationSet,
#                   trainingSet$MVP,
#                   k=k)
#   conf_mat <- table(validationSet$MVP, knn_pred)
#   Acc <- c(Acc, sum(diag(conf_mat))/sum(conf_mat))
# }
# 
# plot(K_list, Acc[-1], ylab = "Accuracy", main = "Accuracy of KNN")
# 
# 
# 
# 
# knn_pred <- knn(trainingSet,
#                 validationSet,
#                 trainingSet$MVP,
#                 k=1)
# 
# table(validationSet$MVP, knn_pred)
# 
# 
# summary(knn_pred)
# 
# which(validationSet$MVP == 1)
# 
# knn_pred <- knn(deservesMVP[,-3],
#                 cans[,-3],
#                 deservesMVP$MVP,
#                 k=1)
# 
# 
# 
# 
# summary(knn_pred)
# cans$MVP <- knn_pred

#Not mine
logistic_model <- glm(MVP~.+I(player_efficiency_rating^2) +(turnover_percentage*player_efficiency_rating) + (WS*seed) + (box_plus_minus*W.L.), 
                      data = trainingSet, 
                      family = "binomial")

# logistic_model <- glm(MVP~.,
#                       data = trainingSet,
#                       family = "binomial")



emptyLM <- glm(MVP~1, trainingSet, family = "binomial")
var_sel <- stepAIC(emptyLM, direction = "forward", scope = list(upper = logistic_model, lower = emptyLM), trace=0)
logistic_model <- var_sel
summary(logistic_model)

predict_reg <- predict(logistic_model, 
                       validationSet, type = "response")


threshHoldList <- seq(0.05, 0.95, 0.05)
Acc <- 0
for (p in threshHoldList){
  predict_reg <- predict(logistic_model, 
                         validationSet, type = "response")
  predict_reg <- ifelse(predict_reg>=p, 1, 0)
  confMat <- table(validationSet$MVP, predict_reg)
  thisAcc <- sum(diag(confMat))/sum(confMat)
  Acc<- c(Acc, thisAcc)
  print(paste(p ," : ", thisAcc))
}
plot(threshHoldList, Acc[-1], ylab = "Accuracy", main = "Logistic Model Accuracy at Different Thresholds")



predict_reg <- predict(logistic_model, 
                       validationSet, type = "response")
# Changing probabilities
predict_reg <- ifelse(predict_reg >=0.75, 1, 0)
# Evaluating model accuracy
# using confusion matrix
table(validationSet$MVP, predict_reg)

missing_MVP <- mean(predict_reg != validationSet$MVP)
print(paste('Accuracy =', 1 - missing_MVP))



##################################################

trainingSet <- deservesMVP[,-3]



logMod <- glm(MVP~.+I(player_efficiency_rating^2) +(turnover_percentage*player_efficiency_rating) + (WS*seed) + (box_plus_minus*W.L.), 
    data = trainingSet, 
    family = "binomial")

summary(logMod)
emptyLM <- glm(MVP~1, trainingSet, family = "binomial")
fullLM <- glm(MVP~., trainingSet, family = "binomial")

var_sel <- stepAIC(emptyLM, direction = "forward", scope = list(upper = logMod, lower = emptyLM))
summary(var_sel)
logistic_model <- var_sel

predict_reg <- predict(var_sel, 
                       cans, type = "response")
predict_reg
# Changing probabilities
predict_reg <- ifelse(predict_reg >=0.5, 1, 0)
predict_reg
# Evaluating model accuracy
# using confusion matrix
table(cans$MVP, predict_reg)


threshHoldList <- seq(0.05, 0.95, 0.1)
Acc <- 0
for (p in threshHoldList){
  predict_reg <- ifelse(predict_reg>=p, 1, 0)
  confMat <- table(cans$MVP, predict_reg)
  Acc<- c(Acc, sum(diag(confMat))/sum(confMat))
}
plot(threshHoldList, Acc[-1], ylab = "Accuracy")











