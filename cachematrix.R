## finds the inverse of a matrix
## use cached output if inverse was calculated previously


## creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize to store matrix
  inverse <- NULL
  
  ##set the inverse to to null when newly called
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  ##set getter and setter methods for inverse
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  
  ##return the list of the matrix
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)

}


## use the cached data to find inverse.
## if inverse was not calculated before find the inverse

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  ##check for cache
  if(!is.null(inverse))
  {
    message("getting cached data")
    return (inverse)
  }
  
  ##if no cahced data, calculate the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  ##return inverse
  inverse
}
