##
## Programming Assignment 2 - R-Programming Course - 
## caching the inverse of a Matrix:
## Matrix inversion is usually a costly computation and at times depending on the scenarios there are some 
## benefits of caching the inverse of a matrix rather than calculate it iteratively. 
## Given below is a set of two functions that can be used to create a special object that 
## stores a matrix and caches its inverse.
##
## This function will create a special object called "Matrix" that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) 
{
  #--  
  invr <- NULL
  set <- function(f) {
    x <<- f
    inv <<- NULL
  }
  get <- function() x
  #--
  setInverse <- function(reverse) invr <<- reverse
  #--
  getInverse <- function() invr
  #--
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  #--
}
## This function will return the inverse of the "matrix" that is created by 
## above function "makeCacheMatrix". If the inverse has already been calculated and if the 
## matrix has not been chang, then it will try to retrieve the inverse from the cache.
cacheSolve <- function(x,...) 
{
  invr <- x$getInverse()
  if (!is.null(invr)) {
    message("Cached data populating")
    return(invr)
  }
  mx <- x$get()
  invr <- solve(mx, ...)
  x$setInverse(invr)
  invr
}
