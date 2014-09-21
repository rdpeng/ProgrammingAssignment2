## These functions are for the second programming assignment in the R Programming
## coursera from Johns Hopkins.  The purpose is to cache potentially time-consuming computations,
## so they don't have be re-calculated.  This work on the matrix inverse coputation.
## Date: 2014 - Sep - 21

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) im <<-inv
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above, and stores in in cache.
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` just retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(m)
  im 
  
}
