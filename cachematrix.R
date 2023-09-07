# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Set the matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    inverse <<- NULL
  }
  
  # Get the matrix
  getMatrix <- function() {
    x
  }
  
  # Get the cached inverse if available; otherwise, calculate and cache it
  cacheSolve <- function(...) {
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    message("Calculating inverse and caching")
    inverse <- solve(x, ...)
    return(inverse)
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheSolve = cacheSolve)
}

# Compute the inverse of the special "matrix" and use caching
cacheSolve <- function(matrixCache, ...) {
  matrixCache$cacheSolve(...)
}
