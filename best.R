

# Finding the best hospital in a state
best <- function(state, outcome) {
        hospital_info <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data_by_state<-split(hospital_info, hospital_info$State)

        State <- data_by_state[[state]]
        #return(m)
        #m<-state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        if(outcome == "heart attack") {
                sorted_data <- State[order(as.numeric(State[,11])),]
                return(head(sorted_data$Hospital.Name, 1))
        }
        else if(outcome == "heart failure") {
                sorted_data <- State[order(as.numeric(State[,17])),]
                return(head(sorted_data$Hospital.Name, 10))
        }
        else if(outcome == "pneumonia") {
                sorted_data <- State[order(as.numeric(State[,23])),]
                return(head(sorted_data$Hospital.Name, 1))
        }
        
}

