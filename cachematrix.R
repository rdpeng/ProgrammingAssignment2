# This code contains 2 functions:
#   - makeCacheMatrix: create an advanced ("special") matrix which also contains its inverse
#   - cacheSolve: compute the inverse of a matrix if the inverse has not been calculated already
#
# usage: 
#   source("cachematrix.R")
#   m <- makeCacheMatrix(matrix(5:8,2))
#   m$get()
#   cacheSolve(m) # compute the inverse of the matrix
#   cacheSolve(m) # retrieve the inverse from cache




## function which creates a special "matrix" object that can cache its inverse:
#
makeCacheMatrix <- function(x = matrix()) {
          I <- NULL
          set <- function(y) {
                  x <<- y
                  I <<- NULL
          }
          get <- function() x
          setInverse <- function(inverse) I <<- inverse
          getInverse <- function() I
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)  
}






## Function which computes the inverse of the special "matrix" returned by  makeCacheMatrix
##   If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve retrieves the inverse from the cache.
#
cacheSolve <- function(x, ...) {
          I <- x$getInverse()
          if(!is.null(I)) {
                  message("getting cached data")
                  return(I)
          }
          data <- x$get()
          I <- solve(data) # Return a matrix that is the inverse of 'x' (i.e. data)
          x$setInverse(I)
          I  
}




