#data proccessing 
df <- read.csv("data/E0(1).csv")
ndf <- df[,c(2:27,49,50)]
write.csv(ndf,"data/infuent_df.csv")
