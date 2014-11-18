table <- read.table("hw1_data.csv",sep=",",stringsAsFactors=FALSE,skip=1)
month <- table$V5
may <- subset.data.frame(table, month == 5)
lapply(may, function (x) x[which.max(x)])