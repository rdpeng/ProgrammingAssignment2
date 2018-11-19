## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
##The following are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }

get <- function( ) x
setinverse <- function (inverse) i <<- inverse
getinverse <- function ( ) i
list (set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}
## Write a short comment describing this function


## The following function computes the inverse of the matrix created by the above
## makeCacheMatrix. 
## If the inverse is calculated and the
## matrix has not changed, then it should retrieve the inverse from the cache


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

