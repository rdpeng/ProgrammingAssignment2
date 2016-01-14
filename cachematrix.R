## These two functions create a matrix-like object capable of calculating 
## and caching its inverse

## This function creates a square matrix-like object capable of caching its
## inverse. It accepts either a matrix or a vector as its input, which it then
## converts into a square matrix if possible.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  rs <- sqrt(length(x))
  create <- function(y) {
    x <<- matrix(y, nrow = rs)
    cm <<- NULL
  }
  retrieve <- function() x
  solv <- function(inverse) cm <<- inverse
  cache <- function() cm
  create(x)
  list(create = create, retrieve = retrieve, solv = solv, cache = cache)
}


## This function first checks for the existence of a cached inverse of 
## the matrix and returns it or calculates one if none is found
## As an input it requires a matrix-like object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  cm <- x$cache()
  if(!is.null(cm)) {
    message("retrieving cached data")
    return(cm)
  }
  work <- x$retrieve()
  cm <- solve(work)
  x$solv(cm)
  cm
}