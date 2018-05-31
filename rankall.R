## Day 1 code. Following Coursera instructions steps 
## custom function to concatenate strings.
"+" = function(BestDataFrame,y) {
  if(is.character(BestDataFrame) || is.character(y)) {
    return(paste(BestDataFrame , y, sep=""))
  } else {
    .Primitive("+")(BestDataFrame,y)
  }
}
## high level path
Courserapath<- getwd() 

## settting individual paths to the 2 coursework files
Courserapath1<-Courserapath + "/Desktop/Nik/Coursera R/Week4/hospital-data.csv"
Courserapath2<-Courserapath + "/Desktop/Nik/Coursera R/Week4/outcome-of-care-measures.csv"

## assignining each file to R dataframe
Nikframe_HD<- read.csv(Courserapath1,header = TRUE)
head(Nikframe_HD)

Nikframe_OOCM<- read.csv(Courserapath2,header = TRUE)
head(Nikframe_OOCM)

## optional commands -used during development-
ncol(Nikframe_OOCM)
names(Nikframe_HD)

## Day 19 code


##assign csv from Niks Mac to the datafile variable
datafile<-"/Users/nik/Desktop/Nik/Coursera R/Week4/outcome-of-care-measures.csv"

##Create rank all Custom function
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  BestDataFrame <- read.csv(file = datafile, colClasses = "character")
  
  ## Check that state and outcome are valid
  colCounter <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)[outcome]
  if (is.na(colCounter)) {
    stop('invalid outcome')
  }
  ## suppress Warnings
  BestDataFrame[, colCounter] <- suppressWarnings(as.numeric(BestDataFrame[, colCounter]))
  ColtoReturn <- 2
  
  ## For each state, find the hospital of the given rank
  ## use lapply on dataframe , Custom function
  findsHospitals <- lapply (split(BestDataFrame, BestDataFrame$State), function(BestDataFrame){
    rawrecords <- BestDataFrame[!is.na(BestDataFrame[colCounter]),] # rawrecords can have no rows
    orderedRecords <- rawrecords[order(rawrecords[colCounter],rawrecords[ColtoReturn]),]
    
    ##convert non numeric input to numeric so it can be used intuitevely in Ranking the Dataframe.
    position <- c('best'=1, 'worst'= nrow(rawrecords))[num]
    if (is.na(position)) {position <- num}
    
    orderedRecords[position, ColtoReturn]
  }
  )
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=unlist(findsHospitals), state=names(findsHospitals))
}
head(rankall("heart attack", 20), 10)