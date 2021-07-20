library(tidymodels)
library(tidyverse)
library(randomForest)
library(caret)
library(readr)
CTG <- read_csv(file.choose())

CTG$NSP <- as.factor(CTG$NSP)
str(CTG)
glimpse(CTG)
skimr::skim(CTG)
table(CTG$NSP)
#Data partition
set.seed(123)
ind <- sample(2, nrow(CTG), replace = TRUE, prob = c(.7, .3))
train <- CTG[ind==1,]
test <- CTG[ind==2,]
#Random forest
set.seed(222)
rf <- randomForest(NSP ~ ., data = train, ntree = 300, mtry=8,
                   importance = TRUE, proximity = TRUE)
rf
attributes(rf)
rf$confusion
rf$predicted
rf$votes
rf$err.rate
#prediction and confusion matrix - train data
p1 <- predict(rf, train)
head(p1)
head(train$NSP)
confusionMatrix(p1, train$NSP)

#prediction and confusion matrix - test data
p2 <- predict(rf, test)
head(p2)
head(test$NSP)
confusionMatrix(p2, test$NSP)
#error rate
plot(rf)
#tune model

#number of tree nodes
hist(treesize(rf), main = "number of nodes")
plot(rf)
varImpPlot(rf, sort = T, n.var = 10,
           main = "TOP 10")
importance(rf)     
varUsed(rf)
#partial plot

getTree(rf, 1, labelVar = TRUE)
getTree(rf, 10, labelVar = TRUE)
MDSplot(rf, train$NSP)

result <- data.frame(test$NSP, predict(rf, test[,-22], type = "response"))
result
plot(result)
