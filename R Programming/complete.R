complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    setwd(paste("/Users/Juan/Documents/DataScience/",directory, sep= ""))
    j <- 0
    comp.data <- data.frame(id= rep(NA,length(id)) ,nobs = rep(NA,length(id)))
    for (i in id) {
        if (i < 10){
            z <-paste("00", i, sep="")
        }else if (10 <= i && i < 100){
            z <- paste("0", i, sep ="")
        }else {
            z <- i
        }
        j <- j+1
        data <- read.csv(paste(z, ".csv", sep =""))
        good <- complete.cases(data)
        nobs <- nrow(data[good,])
        comp.data[j,]<- c(i, nobs)
    }
    comp.data
}