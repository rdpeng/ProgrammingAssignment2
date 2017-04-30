## Functions to calculate the inverse of a matrix and cache the inverse so that
## it can be retrieved without calculating again

## This function caches a matrix and its inverse. Optional input is a matrix.
## Returns list of four functions to get/set an input matrix its corresponding inverse matrix

makeCacheMatrix <- function(inputMatrix = matrix()){
  inverseMatrix <- NULL
  set <- function(y) {
    inputMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() inputMatrix
  setInverse <- function(y) inverseMatrix <<- y
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of a matrix and caches the inverse
## if it was not already cached

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  inputMatrix <- x$get()
  inverseMatrix <- solve(inputMatrix,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
