## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that is capeable of cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x<<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This matrix computes the inverse of the special "matrix" which is returned by makeCacheMatrix. Therefore, if the inverse had been calculated already, then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
