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

