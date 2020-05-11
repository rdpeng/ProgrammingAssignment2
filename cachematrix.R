## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix creates a special "matrix" object that can cache it
## cacheSolve computes the inverse of the special "matrix"
## Write a short comment describing this function
## This function creates a special "matrix" that cab cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
  }


## Write a short comment describing this function
## This function computes the inverse of the special "matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
