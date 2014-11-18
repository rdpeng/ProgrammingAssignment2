## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

    pollutantmean <- function(directory, pollutant, id = 1:332) {

    setwd(paste("/Users/Juan/Documents/DataScience/", directory, sep= ""))
    x <- c()
    for (i in id) {
        if (i < 10){
            z <-paste("00", i, sep="")
        }else if (10 <= i && i < 100){
            z <- paste("0", i, sep ="")
        }else {
            z <- i
        }
        data <- read.csv(paste(z, ".csv", sep =""))
        if (pollutant == "sulfate"){
            y <- data[,2]
        } else if (pollutant == "nitrate") {
            y <- data[,3]
        }
        x <- c(x, y)
    }
    mean_data <- mean (x, na.rm = TRUE)
    round(mean_data, 3)
}