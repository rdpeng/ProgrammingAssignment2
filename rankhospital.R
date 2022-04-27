hospital_info <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data_by_state<-split(hospital_info, hospital_info$State)
State <- data_by_state[["TX"]]
State$Rank <-NA
State[1:10,1:4]

rankhospital <- function(state, outcome, num = "best") {
        hospital_info <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data_by_state<-split(hospital_info, hospital_info$State)
        State <- data_by_state[[state]]
        State$Rank <-NA
        colnames(State)[c(11,17,23)] <- "Rate"
        
        if(outcome == "heart attack") {
                
                sorted_data <- State[order(as.numeric(State[,11]), State[,2]),]
                sorted_data$Rank <- 1:nrow(sorted_data)
                
                if(num == "best") {
                        num <- 1
                }
                else if(num == "worst") {
                        sorted_data$Rate <- as.numeric(sorted_data$Rate)
                        numb <- sum(as.numeric(!is.na(sorted_data$Rate)))
                        print(numb)
                        num <- numb
                }
                
                #print(sorted_data$Rate)
                return(sorted_data[num, 2])
        }
        else if(outcome == "heart failure") {
                sorted_data <- State[order(as.numeric(State[,17]), State[,2]),]
                sorted_data$Rank <- 1:nrow(sorted_data)
                if(num == "best") {
                        num <- 1
                }
                else if(num == "worst") {
                        sorted_data$Rate <- as.numeric(sorted_data$Rate)
                        numb <- sum(as.numeric(!is.na(sorted_data$Rate)))
                        print(numb)
                        num <- numb
                }
                return(sorted_data[num, 2])
        }
        else if(outcome == "pneumonia") {
                sorted_data <- State[order(as.numeric(State[,23]),State[,2]),]
                sorted_data$Rank <- 1:nrow(sorted_data)
                if(num == "best") {
                        num <- 1
                }
                else if(num == "worst") {
                        sorted_data$Rate <- as.numeric(sorted_data$Rate)
                        numb <- sum(as.numeric(!is.na(sorted_data$Rate)))
                        print(numb)
                        num <- numb
                }
                return(sorted_data[num, 2])
        }
        
        
        
        
        
        
}

