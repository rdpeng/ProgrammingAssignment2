pollutantmean <- function(directory = "specdata", pollutant = "sulfate", id = 5:10){
  
  # package is needed to work with dataframes in a list
  require(plyr)
  
  # formating the sequence into file names
  userSelection <- sprintf("%03d.csv", id)
  
  # prepare filenames w.r.t working directory
  filesPrepared <- paste(directory, userSelection, sep = "/")
  
  # prepare a list of dataframes w.r.t each csv file
  listedDFs <- lapply(filesPrepared, read.csv)
  
  
  # merge all dataframes in the list into one dataframe
  finalDF <- ldply(listedDFs)
  
  # calculate mean on finalDF, remove NAS, round to 3 decimals
  round(mean(finalDF[, pollutant], na.rm = TRUE),3)
  
}
