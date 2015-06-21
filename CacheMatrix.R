 
# Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

theData <- c(1.1, 1.2, 2.1, 2.2, 3.1, 3.2) 
mat <- matrix(theData, 2, 3)

cachemean <- function(x,mat) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data)
  x$setmean(m)
  m
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, solve(m) {
  ## Return a matrix that is the inverse of 'x'
}

