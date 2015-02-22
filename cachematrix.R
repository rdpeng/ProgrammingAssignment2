## This file contains a function to calculate Inverse of a matrix and another function to
## Cache and Retrieve the matrix and its inverse.

## This function caches the matrix and its inverse and provides set and get functions
## to store and retrieve the matrices.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse of a matrix to NULL.
  inv <- NULL

  ## Store a matrix and initialize its inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Retrive stored matrix
  get <- function() x
  
  ## Store Inverse of the matrix to cache
  setInverse <- function(inverse) inv <<- inverse
  
  ## Retrieve Inverse of the matrix from cache
  getInverse <- function() inv
  
  ## List of store and retrieve functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gets the inverse of the matrix from cache if available, otherwise, 
## calculates the inverse and stores it in cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Check if inverse of matrix is already in the cache and return result if available
  ##
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    
    ## Returns the inverse
    return(inverse)
  }
  data <- x$get()
  ## Calculate inverse of matrix
  ## Assumption: Matrix is Square and inverse exists (not singular)
  inverse <- solve(data, ...)
  
  ## Save the result into cache
  x$setInverse(inverse)
  
  ## Returns the inverse
  inverse
}
