run_analysis <- function (){
  ## Download the data files
  #fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  #download.file(fileUrl, "getdata-projectfiles-UCI HAR Dataset.zip", mode="wb")
  #unzip("getdata-projectfiles-UCI HAR Dataset.zip")
  
  ## Training data
  trainx <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt", header= FALSE, stringsAsFactors = FALSE)
  trainy <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt", header= FALSE, stringsAsFactors = FALSE)
  
  
  activitylabels <- read.table(".\\UCI HAR Dataset\\activity_labels.txt", header= FALSE, stringsAsFactors = FALSE)
  #dim(trainx)
  
  ## Test data
  testx <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", header= FALSE, stringsAsFactors = FALSE)
  testy <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt", header= FALSE, stringsAsFactors = FALSE)
  #dim(testx)
  
  ## Combine trainging and test data
  testandtrainx <- rbind(trainx, testx)
  testandtrainy <- rbind(trainy, testy)
  activity <- merge(testandtrainy, activitylabels)
  testandtrain <- cbind(testandtrainx, activity[2])
  
  #dim(testandtrainx)
  
  ## Read the column names
  features <-read.table(".\\UCI HAR Dataset\\features.txt", header = FALSE, stringsAsFactors = FALSE)  
  names <- features[2]
  
  ## Set column names
  for(i in 1:561)
    colnames(testandtrainx)[i] <- names[i,1]
  
  meancols <- grep("-mean", colnames(testandtrain))
  stdcols <-  grep("-std", colnames(testandtrain))
  result <- c(meancols, stdcols)
  testandtrain[result]
  
  ## Step 5 TODO
}