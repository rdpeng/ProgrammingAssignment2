complete <- function(directory = "specdata", id = 1:332) {
  
  # formating the sequence into file names
  userSelection <- sprintf("%03d.csv", id)
  
  # prepare filenames w.r.t working directory
  filesPrepared <- paste(directory, userSelection, sep = "/")
  
  # prepare a list of dataframes w.r.t each csv file
  listedDFs <- lapply(filesPrepared, read.csv)
  
  # filter out NAs in the dataframe to get complete cases
  completeDFs <- lapply(listedDFs, na.omit)
  
  # count the number of observations using nrow
  nobs <- sapply(completeDFs, nrow)
  
  # join the 2 vectors to form a dataframe
  data.frame(id,nobs)
  
  
}
