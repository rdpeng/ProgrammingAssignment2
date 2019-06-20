
## This script will cache the inverse of a matrix.

## This function will create a list of functions to set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function solves for a matrix's inverse if it hasn't been cached previously.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data - Inv of Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv      
}
