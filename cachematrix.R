## This does a lot with a Matrix  
## The function creates a special "matrix" object that can cache its inverse.
## it supports setting matrix, getting matrix, setting inverse and getting
## inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }


## Function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already
## done (and the matrix has not changed), then the
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
       message("getting cached data")
  }}