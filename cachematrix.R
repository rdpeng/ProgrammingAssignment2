## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special object that can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  cache <- NULL
  
  # Set the matrix
  set <- function(matrix) {
    x <<- matrix
    cache <<- NULL
  }
  
  # Get the matrix
  get <- function() {
    x
  }
  
  # Set the inverse matrix in the cache
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Get the inverse matrix from the cache
  getInverse <- function() {
    cache
  }
  
  # Return a list containing the set, get, setInverse, and getInverse functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix stored in the object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Check if the inverse matrix is already in the cache
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse matrix")
    return(inverse)
  }
  
  # If not, compute the inverse matrix
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Cache the inverse matrix
  x$setInverse(inverse)
  
  # Return the inverse matrix
  inverse
}

