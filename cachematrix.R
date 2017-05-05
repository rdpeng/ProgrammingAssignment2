## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## it supports aetting matrix, getting matrix, setting inverse, and getting
## inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## the function above (makeCacheMatrix). If the inverse has already been
## calculated and the matrixs has not changed, then the cachesolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(lis.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
