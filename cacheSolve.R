##RProg - 007 Programming Assignment 2 September, 2014
## Student Name: Subhasis Datta sxdatta@gmail.com Student ID: 2955599
## Return a matrix that is the inverse of 'x'
## Cache solve first looks to see if the Inverse matrix is already in the cache
## If not, it creates it and then saves it in cacehe for next time

cacheSolve <- function(x, ...) { 
  ## Look for the Inverse matrix first
  Inv_matrix <- x$getinverse() 
  if(!is.null(Inv_matrix)) { 
    message("Cached data available") 
  ## If found in cache get the Inverse matrix and retrieve it
    return(Inv_matrix) 
  } 
  ## If not in cache, create the cache data with Inverse matrix by getting it
  Cache_data <- x$get() 
  ## create inverse matrix using the solve function
  Inv_matrix <- solve(data) 
  x$setinverse(Inv_matrix) 
  ## Create the Inverse Matrix in the cache and set the values
  Inv_matrix
  ## Return the Inverse matrix
} 
