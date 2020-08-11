df <- read.csv("data/df_with_rating.csv")

summary(df)

df_new <- df[,c(40,41,35,32,10,13,15,16,17,18,19,20,21,22,23,24,25,26,34)]

summary(df_new)

ncol(df_new)

#classification tree
install.packages("caret")
install.packages("car")
install.packages("e1071")
library(caret)

library(car)
library(e1071)

set.seed(190)
train.indices <- createDataPartition(df_new$more_2.5,p=0.8,list=FALSE)

train.df <- df_new[train.indices,]
test.df <-df_new[-train.indices,]
ncol(train.df)
summary(train.df)
summary(test.df)

install.packages("rpart")
library(rpart)
t <- rpart(more_2.5~.,data = train.df)

numFolds <- trainControl(method='cv',number=10)
cpGrid = expand.grid(.cp=seq(0.001,0.05,0.0025))

set.seed(10)
dt.cv <- train(x=train.df[,-19],
               y=train.df$more_2.5,
               method='rpart',
               control=rpart.control(minsplit=10),
               trControl=numFolds,
               tuneGrid=cpGrid)

optimal_cp <- dt.cv$bestTune$cp
optimal_cp

tree1 <- rpart(more_2.5~.,data=train.df,method='class',
               control=rpart.control(minsplit=10,cp=optimal_cp))

tree1
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(tree1)
print(tree1)

tree1.pred <- predict(tree1,newdata = test.df,type='class')
tree1.pred

tree1.cm <- table(true=test.df$more_2.5,predicted=tree1.pred)
tree1.cm

tree1.accuracy <- (sum(diag(tree1.cm))/sum(tree1.cm))*100
tree1.accuracy


install.packages("randomForest")
install.packages("ROCR")
library(ROCR)
library(randomForest)

model1 <- randomForest(more_2.5~.,data=train.df,ntree=500,mtry=6,importance=TRUE)

model1

rf1.pred <- predict(model1,test.df,type='class')
rf1.pred

rf1.cm <- table(predicted=rf1.pred,true=test.df$more_2.5)
rf1.cm

rf1.accuracy <- (sum(diag(rf1.cm))/sum(rf1.cm))*100
rf1.accuracy




#optimize parametars
control <- trainControl(method='repeatedcv',number=10,repeats = 3,search = 'grid')
set.seed(190)
tuneGrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(more_2.5~.,data=train.df,method='rf',tuneGrid=tuneGrid,trControl=control)
plot(rf_gridsearch)
optimal_mtry <- rf_gridsearch$bestTune$mtry

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(190)
custom <- train(more_2.5~., data=train.df, method=customRF, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)

rf_mtry <- custom$bestTune$mtry
rf_ntree <- custom$bestTune$ntree

model2 <- randomForest(more_2.5~.,data=train.df,ntree=rf_ntree,mtry=rf_mtry,importance=TRUE)

model2

rf2.pred <- predict(model2,test.df,type='class')
rf2.pred

rf2.cm <- table(predicted=rf2.pred,true=test.df$more_2.5)
rf2.cm

rf2.accuracy <- (sum(diag(rf2.cm))/sum(rf2.cm))*100
rf2.accuracy

summary(df$true_predicted_goals)

accuracy_by_bet <- (230/380)*100
accuracy_by_bet

install.packages("ISLR")
library(ISLR)

glm.fit <- glm(more_2.5~.,
               data=train.df,
               family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit,
                     newdata=test.df,
                     type='response')

glm.pred <- ifelse(glm.probs>0.5,'Yes','No')
glm.pred

glm.cm <- table(predicted=glm.pred,true=test.df$more_2.5)
glm.cm

glm.accuracy <- (sum(diag(glm.cm))/sum(glm.cm))
glm.accuracy

glm.smaller <- glm(more_2.5~HST+AST+HR,data=train.df,family = binomial)

summary(glm.smaller)

glm.smaller.probs <- predict(glm.smaller,newdata = test.df,type='response')
glm.smaller.pred <- ifelse(glm.smaller.probs>0.5,'Yes','No')

glm.smaller.cm<-table(true=test.df$more_2.5,predicted=glm.smaller.pred)

glm.smaller.accuracy <- (sum(diag(glm.smaller.cm))/sum(glm.smaller.cm))
glm.smaller.accuracy
