table <- read.table("hw1_data.csv",sep=",",stringsAsFactors=FALSE,skip=1)
ozone <- table$V1
temp <- table$V4
values <- subset.data.frame(table, ozone >31 & temp >90)
mean(values$V2)