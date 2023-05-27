## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize an empty variable for storing the inverse
  
  set <- function(y) {
    x <<- y  # Set the value of the matrix
    inv <<- NULL  # Reset the cached inverse since the matrix has changed
  }
  
  get <- function() {
    x  # Retrieve the value of the matrix
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse  # Set the value of the cached inverse
  }
  
  getInverse <- function() {
    inv  # Retrieve the value of the cached inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of a matrix and caches the result

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  
  mat <- x$get()  # Retrieve the matrix from the cache
  inv <- solve(mat, ...)  # Compute the inverse
  
  x$setInverse(inv)  # Cache the inverse
  
  inv  # Return the inverse
}
