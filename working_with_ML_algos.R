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
