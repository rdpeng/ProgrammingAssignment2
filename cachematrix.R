## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function writes calculates the inverse of the matrix created by the makeCacheMatrix function
## above retrieving data from cache if it has already been stored.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
