#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings
#Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
#be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
#and “f” are tied for best, then hospital “b” should be returned).
#The function should use the following template.
#best <- function(state, outcome) {
#  ## Read outcome data
#  ## Check that state and outcome are valid
#  ## Return hospital name in that state with lowest 30-day death
#  ## rate
#}

#The function should check the validity of its arguments. If an invalid state value is passed to best, the
#function should throw an error via the stop function with the exact message “invalid state”. If an invalid
#outcome value is passed to best, the function should throw an error via the stop function with the exact
#message “invalid outcome
#Here is some sample output from the function.
#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "heart attack")
#Error in best("NY", "hert attack") : invalid outcome

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])
data<- outcome[,c(2,7,11,17,23)] #contains only the columns for hospital name, state, heart attack deaths, heart failure deaths and penumonia deaths
names(data)[3]<- "Heart.Attack"
names(data)[4]<- "Heart.Failure"
names(data)[5]<- 'Pneumonia'
head(data)


best<- function(state, outcome){
  st<- data[,2]                #st takes all the abreiveated state names 
  if (state %in% st== FALSE){  #if argument state is not in st, code will throw error
    message('Invalid state')
  }
  else{                       #if the argument state is a valid state name, it will execute the rest of the code    
    dataset<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c(), Heart.Failure=c(), Pneumonia= c()) #dataset is an empty dataframe
    for (i in seq_along(st)) {     #looping through st i.e. all the state names to find the state given by user       
      if (st[i]==state){          #whenever the inputed state is found down the rows, all the coulumns in data are appended into dataset(which was first intitialised as an empty dataframe) 
        placeholder<- data[i,]     
        dataset= rbind(dataset, placeholder)
      }
    }
    placeholder_dataset<- dataset #copying the contents of dataset(which by now is a dataframe containing all the data for the 3 outcomes and hospital names for the given state name entered by user) into placeholder_dataset
    dataset<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c(), Heart.Failure=c(), Pneumonia= c()) #Turning dataset into an empty dataframe again after copying its contents into placeholder_dataset
    
    if (outcome== 'heart attack' || outcome== 'Heart attack'){  #if outcome entered by user is heart attack
      HA<- subset(placeholder_dataset, select = -c(Heart.Failure, Pneumonia)) #dropping the columns for Heart.Failure and Pneumonia
      HA_good<- complete.cases(HA) #removing NA values now
      HA_complete<- HA[HA_good,] #HA_complete is now the dataframe for heart attack without NA values
      HA2<- tapply(HA_complete$Heart.Attack, HA_complete$Hospital.Name, min ) #applying min function to find the best hospital
      best_HA<- which(HA2==min(HA2)) #best_HA is for finding the best hospital after calculating the min values for all hospitals in the state
      print(best_HA) #prints best hospital with the minimum heart attack outcome
      print(min(HA2)) #prints the rate of heart attack outcome in the best hospital in the state
      if(length(best_HA)>1){ #this is the handling if there are ties (i.e. if there are multiple best hospitals found)
        best_list_HA<- sort(HA2) #sorting the HA2 array containg the minimum values for all hospitals in alphabetical order
        best_list_HA[1]  #prints the first ranked hospital in the sorted array in alphabestical order
      }
    }          #Same logic is used if outcomes entered by user are heart failure or Pneumonia down below
    
    else if(outcome== 'heart failure' || outcome== 'Heart failure'){
      HF<- subset(placeholder_dataset, select = -c(Heart.Attack, Pneumonia))
      HF_good<- complete.cases(HF)
      HF_complete<- HF[HF_good,]
      HF2<- tapply(HF_complete$Heart.Failure, HF_complete$Hospital.Name, min )
      best_HF<- which(HF2==min(HF2))
      print(best_HF)
      print(min(HF2))
      if(length(best_HF)>1){
        best_list_HF<- sort(HF2)
        best_list_HF[1]
      }
    }
    else if(outcome== 'Pneumonia'){
      PN<- subset(placeholder_dataset, select = -c(Heart.Attack, Heart.Failure))
      PN_good<- complete.cases(PN)
      PN_complete<- PN[PN_good,]
      PN2<- tapply(PN_complete$Pneumonia, PN_complete$Hospital.Name, min )
      best_PN<- which(PN2==min(PN2))
      print(best_PN)
      print(min(PN2))
      if(length(best_PN)>1){
        best_list_PN<- sort(PN2)
        best_list_PN[1]
      }
    }
    else { #if outcome entered by user is neither of heart attack, heart failure or pneumonia
      message('Outcome is invalid')
    }
    
  }
  
}

best('TX', 'heart attack') #finding best hospital in Texas (TX) for heart attacks. 
#output received: CYPRESS FAIRBANKS MEDICAL CENTER
#correct output: "CYPRESS FAIRBANKS MEDICAL CENTER"

best("TX", "heart failure") #finding best hospital in Texas (TX) for heart failure.
#Output received: HARRIS COUNTY HOSPITAL DISTRICT 
#Correct output should be[1] "FORT DUNCAN MEDICAL CENTER"

best("MD", "heart attack") #finding best hospital for Maryland(MD) for heart attacks
#Output received: JOHNS HOPKINS HOSPITAL, THE 
#Correct output: [1] "JOHNS HOPKINS HOSPITAL, THE"

best("MD", "Pneumonia")
#Output received: CALVERT MEMORIAL HOSPITAL
#Correct output should be[1] "GREATER BALTIMORE MEDICAL CENTER"

best("BB", "heart attack") #this is giving correctly giving error as as the state BB doesn't exist
#Output received: Invalid state 
#