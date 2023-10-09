## Programming assignment 2
# CACHING THE INVERSE OF A MATRIX
# We have to write a pair of functions that cache the inverse of a matrix
# makeCacheMatrix : This function creates a special 'matrix' object that
# can cache its inverse

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# Initialize an empty cache
  cache <- NULL
  
  # Function to set the matrix
  setMatrix <- function(newValue) {
    x <<- newValue  # Use <<- to assign to the parent environment's 'x'
    cache <<- NULL  # Clear the cache when the matrix is updated
  }
  
  # Function to get the matrix
  getMatrix <- function() {
    x
  }
  
  # Function to set the inverse of the matrix in the cache
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the cached inverse if available, otherwise compute and cache it
  getInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached inverse")
      return(cache)
    } else {
      message("Computing inverse and caching")
      inv <- solve(x)
      setInverse(inv)
      return(inv)
    }
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}
}

## CacheSolve function computes the inverse of the special matrix returned by 
# makeCacheMatrix above , If the inverse of the matrix is already been
# calculated

cacheSolve <- function(cacheMatrix) {
  # Check if the cached inverse exists
  if (!is.null(cacheMatrix$getInverse())) {
    return(cacheMatrix$getInverse())
  } else {
    # If not, compute and return the inverse
    message("No cached inverse found. Computing the inverse.")
    inv <- solve(cacheMatrix$getMatrix())
    cacheMatrix$setInverse(inv)
    return(inv)
  }
}
