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

train.indices <- createDataPartition(df_new$more_2.5,p=0.8)
