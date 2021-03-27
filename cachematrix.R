## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

   Inverse <- NULL
     set <- function(y) {
         x <<- y
         Inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inverse <<- inverse
     getinverse <- function() inverse
     list(set = set, get = get,
          setInverse = setinverse,
          getinverse = getinverse)
}}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cachemean <- function(x, ...) {
     inver <- x$getinver()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     inver <- inverse(data, ...)
     x$setinverse(m)
     m
}
