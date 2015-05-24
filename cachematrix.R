## CACHING AN INVERSE OF A MATRIX
## This file caches the inverse of a given matrix so that when the inverse of the same matrix is needed,
## this file will save the unnecessary effort of calculating the inverse again by returning the cached inverse.

## This function creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the matrix received from makeCacheMatrix().
## If the inverse was already cached, retrieve it from the cache, or else calculate the inverse using solve().
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if the inverse has already been calculated and cached, retrieve it from the cache
  if (!is.null(inv)) {
    message("Getting the cached inverse: ")
    return(inv)
  }
  ## if not, then compute the inverse by using Solve() and cache it!
  message("Computing the inverse: ")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
