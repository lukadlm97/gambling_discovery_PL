df <- read.csv("data/prediction_result.csv")

summary(df$HomeTeam)


df$home_predicted_rating <- as.factor(ifelse(df$HomeTeam=='Man City' | df$HomeTeam=='Liverpool' |
                                     df$HomeTeam=='Chelsea' | df$HomeTeam=='Tottenham' | 
                                     df$HomeTeam=='Arsenal','top',
                                     ifelse(df$HomeTeam=='Man United' | df$HomeTeam=='Everton' |
                                              df$HomeTeam=='Leicester' | df$HomeTeam=='Wolves' | 
                                              df$HomeTeam=='Crystal Palace','high',
                                            ifelse(df$HomeTeam=='Bournemouth' | df$HomeTeam=='West Ham' |
                                                     df$HomeTeam=='Watford' | df$HomeTeam=='Newcastle' | 
                                                     df$HomeTeam=='Southampton','mid','low'
                                            )
                                     )))

df$away_predicted_rating <- as.factor(ifelse(df$AwayTeam=='Man City' | df$AwayTeam=='Liverpool' |
                                               df$AwayTeam=='Chelsea' | df$AwayTeam=='Tottenham' | 
                                               df$AwayTeam=='Arsenal','top',
                                             ifelse(df$AwayTeam=='Man United' | df$AwayTeam=='Everton' |
                                                      df$AwayTeam=='Leicester' | df$AwayTeam=='Wolves' | 
                                                      df$AwayTeam=='Crystal Palace','high',
                                                    ifelse(df$AwayTeam=='Bournemouth' | df$AwayTeam=='West Ham' |
                                                             df$AwayTeam=='Watford' | df$AwayTeam=='Newcastle' | 
                                                             df$AwayTeam=='Southampton','mid','low'
                                                    )
                                             )))

summary(df$HomeTeam)
df$home_final_rating <- as.factor(ifelse(df$HomeTeam=='Man City' | df$HomeTeam=='Liverpool' |
                                           df$HomeTeam=='Chelsea' | df$HomeTeam=='Leicester' | 
                                           df$HomeTeam=='Man United','top',
                                         ifelse(df$HomeTeam=='Arsenal' | df$HomeTeam=='Sheffield United' |
                                                  df$HomeTeam=='Tottenham' | df$HomeTeam=='Wolves' | 
                                                  df$HomeTeam=='Burnley','high',
                                                ifelse(df$HomeTeam=='Everton' | df$HomeTeam=='Crystal Palace' |
                                                         df$HomeTeam=='Brighton' | df$HomeTeam=='Newcastle' | 
                                                         df$HomeTeam=='Southampton','mid','low'
                                                )
                                         )))
summary(df)
df$away_final_rating <- as.factor(ifelse(df$AwayTeam=='Man City' | df$AwayTeam=='Liverpool' |
                                           df$AwayTeam=='Chelsea' | df$AwayTeam=='Leicester' | 
                                           df$AwayTeam=='Man United','top',
                                         ifelse(df$AwayTeam=='Arsenal' | df$AwayTeam=='Sheffield United' |
                                                  df$AwayTeam=='Tottenham' | df$AwayTeam=='Wolves' | 
                                                  df$AwayTeam=='Burnley','high',
                                                ifelse(df$AwayTeam=='Everton' | df$AwayTeam=='Crystal Palace' |
                                                         df$AwayTeam=='Brighton' | df$AwayTeam=='Newcastle' | 
                                                         df$AwayTeam=='Southampton','mid','low'
                                                )
                                         )))
df[192,]
table(df$home_predicted_rating,df$HomeTeam)

df$home_predicted_rating <- factor(df$home_final_rating,levels = c("low","mid","high","top"))
df$home_final_rating <- factor(df$home_final_rating,levels = c("low","mid","high","top"))
df$away_predicted_rating <- factor(df$away_predicted_rating,levels = c("low","mid","high","top"))
df$away_final_rating <- factor(df$away_final_rating,levels = c("low","mid","high","top"))

summary(df)
a <- table(df$true_predicted_result,df$true_predicted_goals,df$home_final_rating,df$away_final_rating)
summary(a)

#all down at same time
j = 1
a[64]
predicted_eval <- data.frame()
for(i in 1:16){
  FF <- a[j]
  j = j+1
  TF <- a[j]+a[j+1]
  j = j+2
  TT <- a[j]
  j = j+1
  completly_predicted <- TT/(TT+TF+FF)
  not_completly_predicted <- TF/(TT+TF+FF)
  false_predicted <- FF/(TT+TF+FF)
  predicted_eval <- rbind(predicted_eval,c(completly_predicted,not_completly_predicted,false_predicted))
}

names(predicted_eval)<-c("completly_accurate_prediction","incomplete_prediction","completly_false_prediction")
predicted_eval

a
predicted_eval$finalyrank <- NA
predicted_eval[1,4] <- 'low vs low'
predicted_eval[2,4] <- 'mid vs low'
predicted_eval[3,4] <- 'high vs low'
predicted_eval[4,4] <- 'top vs low'

predicted_eval[5,4] <- 'low vs mid'
predicted_eval[6,4] <- 'mid vs mid'
predicted_eval[7,4] <- 'high vs mid'
predicted_eval[8,4] <- 'top vs mid'

predicted_eval[9,4] <- 'low vs high'
predicted_eval[10,4] <- 'mid vs high'
predicted_eval[11,4] <- 'high vs high'
predicted_eval[12,4] <- 'top vs high'

predicted_eval[13,4] <- 'low vs top'
predicted_eval[14,4] <- 'mid vs top'
predicted_eval[15,4] <- 'high vs top'
predicted_eval[16,4] <- 'top vs top'

predicted_eval
write.csv(predicted_eval,"data/accuracy_of_prediction_by_rank.csv")

library(ggplot2)
names(predicted_eval)<-c("completly_accurate_prediction","incomplete_prediction","completly_false_prediction","finaly_rank")
predicted_eval$finaly_rank <- factor(predicted_eval$finaly_rank,levels = predicted_eval$finaly_rank[order(predicted_eval$completly_accurate_prediction)])

ggplot(predicted_eval,aes(x=finaly_rank,y=completly_accurate_prediction))+geom_point(size=4)
