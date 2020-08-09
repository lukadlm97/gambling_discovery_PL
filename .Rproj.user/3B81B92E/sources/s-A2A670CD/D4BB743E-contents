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

