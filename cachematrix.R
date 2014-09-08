## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly.
## These two functions cache the inverse of a matrix, and calculate it only if the matrix has changed.

## makeCacheMatrix gets a matrix and returns a list of functions and their associated environment

makeCacheMatrix <- function(TheMatrix = numeric()) {
  # defines 4 functions:
  # set - set a new matrix
  # get - read the stored matrix
  # setinverse - store the new inverse
  # getinverse - read the stored inverse
  # notice that this function does not do any calculation by itself.
  # Also, the functions defined by it, perform only read/write operations.
  # The inverse calculation and all 'decision making' are done in cachesolve  
  
  TheInverse <- NULL
  set <- function(NewMatrix) {
    TheMatrix <<- NewMatrix
    TheInverse <<- NULL
  }
  get <- function() TheMatrix
  setinverse <- function(NewInverse) TheInverse <<- NewInverse
  getinverse <- function() TheInverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # The output of the function
}

## cachesolve gets an object ('TheObject').
## The object points to (or 'contain') an environment of 4 functions+variables that was the output of makeCacheMatrix.

cacheSolve <- function(TheObject, ...) {
  TheInverse <- TheObject$getinverse()
  if(!is.null(TheInverse)) {
    message("getting cached data")
    return(TheInverse) 
  }
  data <- TheObject$get()
  TheInverse <- solve(data, ...) #Only at this step the inverse is calculated
  TheObject$setinverse(TheInverse) #update the calculated inverse in 'TheMatrix' environment
  TheInverse #return (or print out) the inverse
}