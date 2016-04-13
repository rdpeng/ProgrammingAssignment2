setwd("D:/Coursera/Lecture 4/rprog-data-ProgAssignment3-data")

##########################Reading the Data#####################################

CareMeasureDB <- read.csv("outcome-of-care-measures.csv",header = TRUE, stringsAsFactors = FALSE, na.strings = "Not Available" )
head(CareMeasureDB)
names(CareMeasureDB)

##########################Selecting Columns#####################################

CareMeasure_Combined <- CareMeasureDB[, c(2, 7, 11, 17, 23)]
names(CareMeasure_Combined) <- c("Hospital", "State", "Outcomes_HeartAttack", "Outcomes_HeartFailure", "Outcomes_Pneumonia")
head(CareMeasure_Combined)
str(CareMeasure_Combined)
names(CareMeasure_Combined)

#Subsetting

CareMeasure_Subset_HeartAttack <- subset(CareMeasure_Combined, select = c(Hospital, State, Outcomes_HeartAttack))
CareMeasure_Subset_HeartFailure <- subset(CareMeasure_Combined, select = c(Hospital, State, Outcomes_HeartFailure))
CareMeasure_Subset_Pneumonia <- subset(CareMeasure_Combined, select = c(Hospital, State, Outcomes_Pneumonia))


##########################remove NAs#####################################

CareMeasure_Subset_HeartAttack<- CareMeasure_Subset_HeartAttack[complete.cases(CareMeasure_Subset_HeartAttack), ]
CareMeasure_Subset_HeartFailure<- CareMeasure_Subset_HeartFailure[complete.cases(CareMeasure_Subset_HeartFailure), ]
CareMeasure_Subset_Pneumonia<- CareMeasure_Subset_Pneumonia[complete.cases(CareMeasure_Subset_Pneumonia), ]

CareMeasure_Subset_HeartAttack <- cbind(CareMeasure_Subset_HeartAttack, "heart attack")
CareMeasure_Subset_HeartFailure <- cbind(CareMeasure_Subset_HeartFailure, "heart failure")
CareMeasure_Subset_Pneumonia <- cbind(CareMeasure_Subset_Pneumonia, "pneumonia")

names(CareMeasure_Subset_HeartAttack) <- c("Hospital", "State", "Outcome Number", "Outcomes")
names(CareMeasure_Subset_HeartFailure) <- c("Hospital", "State", "Outcome Number", "Outcomes")
names(CareMeasure_Subset_Pneumonia) <- c("Hospital", "State", "Outcome Number", "Outcomes")

CareMeasure_Merge <- rbind(CareMeasure_Subset_HeartAttack, CareMeasure_Subset_HeartFailure, CareMeasure_Subset_Pneumonia)

dim(CareMeasure_Merge)
head(CareMeasure_Merge)


#write.csv(CareMeasure_Merge, "Merge.csv")

##########################Sorting#####################################


CareMeasure_Sorted <- CareMeasure_Merge[unique(order(CareMeasure_Merge$State, CareMeasure_Merge$`Outcome Number`,
                                                     CareMeasure_Merge$Outcomes,  
                                                     CareMeasure_Merge$Hospital)),]
dim(CareMeasure_Sorted)
head(CareMeasure_Sorted)
unique(CareMeasure_Sorted$State)  #unique - 54

#write.csv(CareMeasure_Sorted, "Sorted.csv")


##########################Ranking#####################################

#subset states

rankState <- as.data.frame(matrix(NA, nrow = nrow(CareMeasure_Sorted)))
names(rankState) <- c("rankState")
#rankState <- data.frame(ncol = rankState, ncol = 1)

CareMeasure_Sorted <- cbind(rankState, CareMeasure_Sorted)


StateNo <- length(unique(CareMeasure_Sorted$State))
State_Groups <- unique(CareMeasure_Sorted$State)
CareMeasure_Rank_Sorted = data.frame()

for(i in 1:StateNo){
   # CareMeasure_Rank = data.frame()
   CareMeasure_Rank <- subset(CareMeasure_Sorted, CareMeasure_Sorted$State == State_Groups[i])
   OutcomeNo = length(unique(CareMeasure_Rank$Outcomes))
   Outcome_Groups = unique(CareMeasure_Rank$Outcomes)
   for(j in 1:OutcomeNo){
     CareMeasure_Rank_Out <- subset(CareMeasure_Rank, CareMeasure_Rank$Outcomes == Outcome_Groups[j])
     #CareMeasure_Rank_Out <- CareMeasure_Rank_Out[sort(CareMeasure_Rank_Out$`Outcome Number`),]
      CareMeasure_Rank_Out$rankState[2:nrow(CareMeasure_Rank_Out)-1] = 2:nrow(CareMeasure_Rank_Out)-1
     CareMeasure_Rank_Out$rankState[1] = "best"
     CareMeasure_Rank_Out$rankState[nrow(CareMeasure_Rank_Out)] = "worst"
     #CareMeasure_Rank = rbind(CareMeasure_Rank, CareMeasure_Rank_Out)
     CareMeasure_Rank_Sorted = rbind(CareMeasure_Rank_Sorted, CareMeasure_Rank_Out)
   }
}

# CareMeasure_Rank_S = data.frame()
#   # CareMeasure_Rank = data.frame()
#   CareMeasure_R <- subset(CareMeasure_Sorted, CareMeasure_Sorted$State == "AK")
#   
#   OutcomeNo = length(unique(CareMeasure_R$Outcomes))
#   Outcome_Groups = unique(CareMeasure_R$Outcomes)
#   
#     CareMeasure_RO <- subset(CareMeasure_R, CareMeasure_R$Outcomes == "heart attack")
#     RowNum <- 2:nrow(CareMeasure_RO)-1
#     CareMeasure_RO$rankState[2:nrow(CareMeasure_RO)-1] = RowNum - 1
#     CareMeasure_RO$rankState[1] = "best"
#     CareMeasure_RO$rankState[nrow(CareMeasure_RO)] = "worst"
#     #CareMeasure_R = rbind(CareMeasure_R, CareMeasure_RO)
#  
#     CareMeasure_Rank_S = rbind(CareMeasure_Rank_S, CareMeasure_RO)


head(CareMeasure_Rank_Sorted,100)
tail(CareMeasure_Rank_Sorted)
class(CareMeasure_Rank_Sorted$rankState)
dim(CareMeasure_Rank_Sorted)

##########################Data Processing#####################################


splitdata <- split(CareMeasure_Rank_Sorted, CareMeasure_Rank_Sorted$State)
# splitdata[[15]][[2]]
#splitdata_new <- as.data.frame(splitdata)

sapply(splitdata,length)

state_split <- unlist(lapply(splitdata, "[[", 3))
hospital_split <- unlist(lapply(splitdata, "[[", 2))
outcomes_split <- unlist(lapply(splitdata, "[[", 5))
rank_split <- unlist(lapply(splitdata, "[[", 1))

#DataNamed <- data.frame(state = state_split, hospital = hospital_split, outcomes = outcomes_split, rank = rank_split)
# #XX <- data.frame(hospital = Hos, State = StatName, row.names = StatName)
# head(DataNamed)

#write.csv(DataNamed, "DataNamed.csv")

#Questions

###########best function#########

best <- function(st, ou){
  index_x = which(state_split == st & outcomes_split == ou & rank_split == "best")
  if(rank_split == index_x) hospital = hospital_split[index_x]
  else if(rank_split == "best") hospital = hospital_split[index_x]
  else if(rank_split == "worst") hospital = hospital_split[index_x]
  hospital
}

best("SC", "heart attack")  #Q1
best("NY", "pneumonia")     #Q2
best("AK", "pneumonia")     #Q3

###########rankhospital function#########

rankhospital  <- function(st, ou, rk){
  index_x = which(state_split == st & outcomes_split == ou & rank_split == rk)
  if(rank_split == index_x) hospital = hospital_split[index_x]
  else if(rank_split == "best") hospital = hospital_split[index_x]
  else if(rank_split == "worst") hospital = hospital_split[index_x]
  hospital
}

rankhospital("NC", "heart attack", "worst")    #Q4
rankhospital("WA", "heart attack", 7)          #Q5
rankhospital("TX", "pneumonia", 10)            #Q6
rankhospital("NY", "heart attack", 7)          #Q7

###########rankall function#########

rankall  <- function(ou, rk){
  index_x = which(outcomes_split == ou & rank_split == rk)
  if(rank_split == index_x){ 
    state = state[index_x]
    hospital = hospital_split[index_x]
  }
  else if(rank_split == "best") {
    state = state_split[index_x]
    hospital = hospital_split[index_x]
  }
  else if(rank_split == "worst") {
    state = state_split[index_x]
    hospital = hospital_split[index_x]
  }
  dt = as.data.frame(cbind(state, hospital))
}

r <- rankall("heart attack", 4)                    #Q8
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")                 #Q9
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)                  #Q10
as.character(subset(r, state == "NV")$hospital)




