## Week3: R_Programming Assignment 2 - N.S.

## The following 2 functions wants to create a special objects that
## stores a matrix and caches its invers

## makeCacheMatrix is the first function and wants to create a special
## Matrix object that cache its invers:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
}

## cacheSolve is the second function. Its scope is to compute the 
## invers of the special Matrix created by makeCacheMatrix. However,
## if the invers has already been calculated then the function retrieve
## the function from the cache and skips the computation. If this is
## not the case: it calculates the invers of the matrix and sets the 
## invers of the metrix in the cache.

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
