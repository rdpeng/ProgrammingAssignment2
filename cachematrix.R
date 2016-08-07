## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix returns an object that allows storing of 2 matrices using the functions:
## $set(y) resets matrices and saves the main one
## $get() returns the saved main matrix
## $setinv(y) stores secondary matrix
## $getinv() returns secondary matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(y) z <<- y
  getinv <- function() z
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## (assuming that the matrix supplied is always square)
## x must be an object created with makeCacheMatrix 
## use $set to store the value into the object before using CacheSolve

cacheSolve <- function(x, ...) {
  z <- x$getinv()
  if (!is.null(z)) {
    message("Getting cached data")
    return(z)
  }
  z <- solve(x$get(), ...)
  x$setinv(z)
  z
}
