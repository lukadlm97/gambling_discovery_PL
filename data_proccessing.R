#data proccessing II part
df <- read.csv("data/infuent_df.csv")
summary(df)

#month proccessing
df$month <- NA
ncol(df)
df[1,30]<-3
for(i in 1:nrow(df)){
  df[i,30] <- month.name[as.numeric(substr(df[i,2],4,5))]
}
t <- as.factor(df$month)
t
df$month <- NA
df$month <- t
summary(df$month)
df$c <- NULL
df$month <- factor(df$month,levels=c("August","September","October","November","December","January","February","March","June","July"))

install.packages("ggplot2")
library(ggplot2)
#ploting count of matchs by MONTH
a <- summary(df$month)
count_of_matchs = c(1:10)
for(i in 1:10){
  count_of_matchs[i] <- a[[i]]
}
all_month = df$month
all_month <- c("August","September","October","November","December","January","February","March","June","July")
all_month
new_df <- as.data.frame(cbind(all_month,count_of_matchs))
names(new_df)<-c("month","matches")

#sort by value
new_df$month <- factor(new_df$month,levels = new_df$month[order(new_df$matches)])
ggplot(new_df,aes(x=month,y=matches,fill=new_df$month))+geom_bar(stat="identity")+
          xlab("Month of match")+
          ylab("Number of matches in month")+
          ggtitle("Number of matches by month")+
          scale_fill_manual("legend",values = new_df$month)
          



#sort by month
new_df$month <- factor(new_df$month,levels = new_df$month)
ggplot(new_df,aes(x=month,y=matches,fill=new_df$month))+geom_bar(stat="identity")+
  xlab("Month of match")+
  ylab("Number of matches in month")+
  ggtitle("Number of matches by month")+
  scale_fill_manual("legend",values = new_df$month)

summary(df)

# working on total goals
df$FTAG[199]
df$total_goals <- NA
for(i in 1:nrow(df)){
  df[i,31]<-df[i,6]+df[i,7]
}

df$more_2.5 <- NA
df$more_2.5 <- as.factor(ifelse(df$total_goals>=2.5,'Yes','No'))
df$predited_more_2.5 <- as.factor(ifelse(df$B365.2.5>=df$B365.2.5.1,'No','Yes'))
df$true_predicted_goals <- as.factor(ifelse(df$more_2.5==df$predited_more_2.5,'Yes','No')) 
summary(df)

summary(df$HomeTeam)

true_predicted_rate <- data.frame()

liverpool_df <- subset(df,df$HomeTeam=='Liverpool' | df$AwayTeam=='Liverpool')
summary(liverpool_df)
true_predicted <- sum(liverpool_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(c('Liverpool',as.numeric(true_predicted)))
true_predicted_rate

city_df <- subset(df,df$HomeTeam=='Man City' | df$AwayTeam=='Man City')
summary(city_df)
true_predicted <- sum(city_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Man City',as.numeric(true_predicted)))
true_predicted_rate

newcastel_df <- subset(df,df$HomeTeam=='Newcastle' | df$AwayTeam=='Newcastle')
summary(newcastel_df)
true_predicted <- sum(newcastel_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Newcastle',as.numeric(true_predicted)))
true_predicted_rate

wolves_df <- subset(df,df$HomeTeam=='Wolves' | df$AwayTeam=='Wolves')
summary(wolves_df)
true_predicted <- sum(wolves_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Wolves',as.numeric(true_predicted)))
true_predicted_rate

watford_df <- subset(df,df$HomeTeam=='Watford' | df$AwayTeam=='Watford')
summary(watford_df)
true_predicted <- sum(watford_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Watford',as.numeric(true_predicted)))
true_predicted_rate

norwich_df <- subset(df,df$HomeTeam=='Norwich' | df$AwayTeam=='Norwich')
summary(norwich_df)
true_predicted <- sum(norwich_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Norwich',as.numeric(true_predicted)))
true_predicted_rate

west_ham_df <- subset(df,df$HomeTeam=='West Ham' | df$AwayTeam=='West Ham')
summary(west_ham_df)
true_predicted <- sum(west_ham_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('West Ham',as.numeric(true_predicted)))
true_predicted_rate

tottenham_df <- subset(df,df$HomeTeam=='Tottenham' | df$AwayTeam=='Tottenham')
summary(tottenham_df)
true_predicted <- sum(tottenham_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Tottenham',as.numeric(true_predicted)))
true_predicted_rate

southampton_df <- subset(df,df$HomeTeam=='Southampton' | df$AwayTeam=='Southampton')
summary(southampton_df)
true_predicted <- sum(southampton_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Southampton',as.numeric(true_predicted)))
true_predicted_rate

sheffield_united_df <- subset(df,df$HomeTeam=='Sheffield United' | df$AwayTeam=='Sheffield United')
summary(sheffield_united_df)
true_predicted <- sum(sheffield_united_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Sheffield United',as.numeric(true_predicted)))
true_predicted_rate

man_united_df <- subset(df,df$HomeTeam=='Man United' | df$AwayTeam=='Man United')
summary(man_united_df)
true_predicted <- sum(man_united_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Man United',as.numeric(true_predicted)))
true_predicted_rate

leicester_df <- subset(df,df$HomeTeam=='Leicester' | df$AwayTeam=='Leicester')
summary(leicester_df)
true_predicted <- sum(leicester_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Leicester',as.numeric(true_predicted)))
true_predicted_rate

everton_df <- subset(df,df$HomeTeam=='Everton' | df$AwayTeam=='Everton')
summary(everton_df)
true_predicted <- sum(everton_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Everton',as.numeric(true_predicted)))
true_predicted_rate

crystal_palace_df <- subset(df,df$HomeTeam=='Crystal Palace' | df$AwayTeam=='Crystal Palace')
summary(crystal_palace_df)
true_predicted <- sum(crystal_palace_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Crystal Palace',as.numeric(true_predicted)))
true_predicted_rate

chelsea_df <- subset(df,df$HomeTeam=='Chelsea' | df$AwayTeam=='Chelsea')
summary(chelsea_df)
true_predicted <- sum(chelsea_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Chelsea',as.numeric(true_predicted)))
true_predicted_rate

burnley_df <- subset(df,df$HomeTeam=='Burnley' | df$AwayTeam=='Burnley')
summary(burnley_df)
true_predicted <- sum(burnley_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Burnley',as.numeric(true_predicted)))
true_predicted_rate

brighton_df <- subset(df,df$HomeTeam=='Brighton' | df$AwayTeam=='Brighton')
summary(brighton_df)
true_predicted <- sum(brighton_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Brighton',as.numeric(true_predicted)))
true_predicted_rate

bournemouth_df <- subset(df,df$HomeTeam=='Bournemouth' | df$AwayTeam=='Bournemouth')
summary(bournemouth_df)
true_predicted <- sum(bournemouth_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Bournemouth',as.numeric(true_predicted)))
true_predicted_rate

arsenal_df <- subset(df,df$HomeTeam=='Arsenal' | df$AwayTeam=='Arsenal')
summary(arsenal_df)
true_predicted <- sum(arsenal_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Arsenal',as.numeric(true_predicted)))
true_predicted_rate

aston_villa_df <- subset(df,df$HomeTeam=='Aston Villa' | df$AwayTeam=='Aston Villa')
summary(aston_villa_df)
true_predicted <- sum(aston_villa_df$true_predicted_goals=='Yes')/38
true_predicted_rate <- rbind(true_predicted_rate,c('Aston Villa',as.numeric(true_predicted)))
true_predicted_rate

true_predicted_rate
true_predicted_rate[,1]

matrix_of_accuracy_of_prediction <- data.frame(as.factor(true_predicted_rate[,1]),as.numeric(true_predicted_rate[,2]))

names(matrix_of_accuracy_of_prediction)<-c("Team_name","Accuracy")
matrix_of_accuracy_of_prediction

ggplot(matrix_of_accuracy_of_prediction,aes(x=Team_name,y=Accuracy))+geom_point()
