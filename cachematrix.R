## Week 3 Assignment(Lexical Scoping); 

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function creates a special matrix that cache the inverse of the input matrix

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


## Write a short comment describing this function
## The below function creates an inverse of the special matrix which is returned by
## the function makeCacheMatrix. If the inverse is already calculated, then
## cacheSolve will restore the inverse of cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }
