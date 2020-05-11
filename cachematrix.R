#############################################
# Programming Assignment 2: Lexical Scoping #
#############################################

# Caching the Inverse of a Matrix:
# Matrix inversion is generally a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it repeatedly.
# Here are a pair of functions that work in conjunction to create a special object
# that stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" created by the 
# aforementioned function 'makeCacheMatrix'. If the inverse has already been
# computed (and the matrix has not changed), then it should retrieve the 
# inverse stored in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # returns a matrix that is inverse of x
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
