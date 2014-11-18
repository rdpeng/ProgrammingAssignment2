table <- read.table("hw1_data.csv",sep=",",stringsAsFactors=FALSE,skip=1)
month <- table$V5
june <- subset.data.frame(table, month == 6)
temp <- june$V4
mean(temp)