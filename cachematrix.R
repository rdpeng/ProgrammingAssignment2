## Put comments here that give an overall description of what your
## functions do

## Solution prepared by Marcin Borucki.
## Self developed solution based on a provided example of vector cache means.

## Solving for matrix invert and caching the result in the special matrix object.
## The goal is to save computation time for potentially invertig the same matrix multiple times.


## Write a short comment describing this function

## Function makeCacheMatrix will create an object to store matrix.
## and it invert in cache only for invertable matrices.
## To calculate the invert and stor it in the object call cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve function will solve for an invert of a matrix and store the invert
## in the object created by makeCacheMatrix for reuse when invoked later.
## The matrix needs to be invertible and storded in makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
