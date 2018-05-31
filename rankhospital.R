"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}
## high level path
Courserapath<- getwd() 

## settting individual paths to the coursework files
Courserapath2<-Courserapath + "/Desktop/Nik/Coursera R/Week4/outcome-of-care-measures.csv"

rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  
  BestDataFrame<- read.csv(file=Courserapath2,na.strings = "Not Available",colClasses = "character",stringsAsFactors = FALSE)[ ,c(2,7,11,17,23)]
  ## need to set ur NA string.. cause not all data sets are coming with "NA" (they may come us 'NAN', 'NotA' etc etc)
  ## without the na.string parameter set then functions which removes na(na.rm=TRUE) wont work
  
  colnames(BestDataFrame) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% BestDataFrame[, "state"]) {
    stop('Invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('Invalid headers')} 
  
  ##  check if rank is numeric input or string
  # If is indeed numeric proceed with the following logic
  if (is.numeric(rank)) {
    dfState <- which(BestDataFrame[, "state"] == state)
    dfallStaterecords <- BestDataFrame[dfState, ]   # define dataframe records for the desired state
    dfallStaterecords[, eval(outcome)] <- as.numeric(dfallStaterecords[, eval(outcome)])
    dfallStaterecords <- dfallStaterecords[order(dfallStaterecords[, eval(outcome)], dfallStaterecords[, "hospital"]), ]
    result <- dfallStaterecords[, "hospital"][rank] # rank calls the pre ordered record we wish to return
  } 
  
  # If is not numeric proceed with the following logic  
  if (!is.numeric(rank)){
    #If function input argument is best then reduces to previous function. 
    if (rank == "best") {
      result <- best(state, outcome)
      #If function input argument is worst then  proceed with the following logic 
    } else if (rank == "worst") {
      dfState <- which(BestDataFrame[, "state"] == state)
      dfallStaterecords <- BestDataFrame[dfState, ]    
      dfallStaterecords[, eval(outcome)] <- as.numeric(dfallStaterecords[, eval(outcome)])
      dfallStaterecords <- dfallStaterecords[order(dfallStaterecords[, eval(outcome)], dfallStaterecords[, "hospital"], decreasing = TRUE), ]
      result <- dfallStaterecords[, "hospital"][1]
    } else {
      stop('invalid rank')  #  residual case error message.
    }
  }
  return(result)
}

# checking results->
rankhospital("MD", "heart attack", "worst")
