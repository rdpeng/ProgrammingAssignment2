## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##test the github
  cachemean<- function(x, ...){
    m<- x$getmean()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
      
    }
    data<- x$get()
    m <-mean(data, ...)
    x$setmean(m)
    m
  }
  
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
