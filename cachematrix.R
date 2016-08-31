## These functions support a special matrix that can cache the value of it's inverse
## so that this costly calculation does not need to be performed multiple times
## on the same matrix.

## The makeCacheMatrix function is applied to a given matrix and it turns it 
## into a cacheMatrix which has useful functions that can be accessed via 
## functions embedded within the cacheMatrix.  These functions include
## getting and setting the cacheMatrix and getting and setting the cacheMatrix
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is intended to be along with the makeCacheMatrix function.
## The function is used to return the inverse of the cacheMatrix.  If the inverse
## of the cacheMatrix is already assigned it will be stored within the
## cacheMatrix and this function will merely return the inverse.  If the 
## cacheMatrix inverse has not yet been calculated, this function will calculate
## the inverse and store it within the cacheMatrix for future retrieval.  In
## either case the function also returns the cacheMatrix inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
