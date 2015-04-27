corr <- function(directory, threshold = 0) {
  
  id <- 1:332
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
  summaryDF <- data.frame(id,nobs)
  
  # form a datafram with only threshold met monitors metadata (id,nobs)
  thresholdMetMonitors <- summaryDF[summaryDF$nobs >= threshold, ]
  
  if(nrow(thresholdMetMonitors) != 0) {
    
    MetDFs <- completeDFs[thresholdMetMonitors$id]
    
    out <- c(1:length(MetDFs))
    
    #print(out)
    
    #MetDFs[[1]]$sulfate
    
    for(i in 1:length(MetDFs)) {
      #print(i)
      #print(cor(MetDFs[[i]]$sulfate, MetDFs[[i]]$nitrate))
      out[i] <- cor(MetDFs[[i]]$sulfate, MetDFs[[i]]$nitrate)
      #print(out[i])
    }
    
    out
  } else { numeric(0) }
  #out
}
