## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to-
##-set values of the Matrix
##-get values in the Matrix
##-set the inverse of the matrix
##-get values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL
}
get <- function() x
setinv <- function(inv) xinv <<- inv
getinv <- function() xinv


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- solve(x)
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- inv(data, ...)
  x$setinv(xinv)
  xinv
}
