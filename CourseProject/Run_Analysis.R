# Download the files into the local directory.
# Upon analysis, there are two main folders - Test / Train which contain the files for the required purpose.
# the main folder has the readme, features.txt and activity labels which are requried for the assignment

# First, Perform the operations on Test Folder. 
# Certain tasks can be resused for Train Datasets. 

#1) Include library dplyr, tidyr

library(tidyr)
library(dplyr)

# 2. Read the Test Data Set one by one
X <- read.table("./Prj/UCI HAR Dataset/test/X_test.txt", header = FALSE)
dim(X)
Y <- read.table("./Prj/UCI HAR Dataset/test/Y_test.txt", header = FALSE)
dim(Y)
S <- read.table("./Prj/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
dim(S)
# 3At this point all the files are loaded into R, as seperate tables. all of them have 2947 observations.

# 4. only choose required columns using the Contains Option

l <- read.table("./Prj/UCI HAR Dataset/features.txt", header = FALSE)
l$V2 <- make.names(l$V2,unique=TRUE,allow_=TRUE)
l <- tbl_df(l)
colnames(X) <- l$V2

Xtest <- cbind(select(X,contains(c("std"))),select(X,contains(c("mean"))))
# 5. Covert the data sets into tbl_df, so that we can tidy up the data.
Test_Dataset <- tbl_df(Xtest)

# 6. Validate this data set. Check row count is same.

# 7. Add Descriptive Variable Name. Find columns with mean and std, and then create a table of the same.

t <- l[grep("mean",l$V2,ignore.case = TRUE),2]
t1 <- l[grep("std",l$V2,ignore.case = TRUE),2]
lbl <- rbind(t,t1)

# 8 Write out the variable names into a XLS file

write.xlsx(lbl,"label.xls",col.names = TRUE)

# 9 Update the Variable name in XLS and upload back into another data frame. 

newlbl <- read.xlsx("labels.xls",sheetIndex = 1,header=TRUE)

# 10. assign the Variable name using colnames function.

colnames(Test_Dataset) <- newlbl$Descriptive_Names

# 11 cbind the tables to include all the columns in one table

colnames(S) <- "Subject"
Test_Dataset <- cbind(Test_Dataset,S)


# 12 Converty Activity Type to be Descriptive.
# 13 Include Spatstat Package to enable use of Lut Function.

library(spatstat)
Actlbl <- lut(c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"),inputs = c(1,2,3,4,5,6))

colnames(Y) <- "ActivityType"
Y$ActivityType <- Actlbl(Y$ActivityType)
Test_Dataset <- cbind(Test_Dataset,Y)



#************************Train Dataset

X<- read.table("./Prj/UCI HAR Dataset/Train/X_train.txt", header = FALSE)
dim(X)
Y <- read.table("./Prj/UCI HAR Dataset/Train/Y_train.txt", header = FALSE)
dim(Y)
S <- read.table("./Prj/UCI HAR Dataset/Train/subject_train.txt", header = FALSE)
dim(S)

colnames(X) <- l$V2
Xtrain <- cbind(select(X,contains(c("std"))),select(X,contains(c("mean"))))
# Covert the data sets into tbl_df, so that we can tidy up the data.
Train_Dataset <- tbl_df(Xtrain)
colnames(Train_Dataset) <- newlbl$Descriptive_Names
colnames(S) <- "Subject"
Train_Dataset <- cbind(Train_Dataset,S)
colnames(Y) <- "ActivityType"
Y$ActivityType <- Actlbl(Y$ActivityType)
Train_Dataset <- cbind(Train_Dataset,Y)

# Merge the two Datasets
# both  the datasets have 88 columns and then different number of observations. All are now combined into one Dataset.

HumanActivity_Dataset <- rbind(Train_Dataset,Test_Dataset)

# Another independent Data set , removing Variables for Gyro, or one's containing XYZ Axis measurmenets, Jerk Signals and Frequency Signals
# Resultant Data set has only Acceleration Time Domain Signals for Analysis. 
# We can create multiple subsets for each of these types and establish co-relation with one another. 


HA_Analysis_Dataset <- HumanActivity_Dataset %>% select(-contains("Gyro")) %>% 
  select(-contains("axis")) %>% select(-contains("Jerk")) %>% select(-contains("F"))

# Group by Activity Type and Subject
HA_Analysis_Grp_Dataset <-HA_Analysis_Dataset%>% group_by(ActivityType,Subject)%>%
  summarize_each(c("mean"))
View(HA_Analysis_Grp_Dataset)

#Same logic can be applied on the HumanACtivity_Dataset, if we want to have the average of all the variables by Activity Type and Subjects
Final_Dataset_all <- HumanActivity_Dataset %>% group_by(Subject,ActivityType) %>% summarize_each(c("mean"))
View(Final_Dataset_all)

# Write the output of the last Dataset into a txt file, with row names = FALSE
write.table(Final_Dataset_all,file ="Final.txt",row.names = FALSE)










