## Utilities for calculating and storing inverse of a martix. By caching the results,
## functions avoid the cost of calculating inverse again. 

## Creates and returns a matrix that caches inverse of a matrix
## Input: Matrix, Returns: Cached Matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Calculates/Returns the inverse of a matrix.
## If inverse is cached, returns cached inverse of the matrix
## Else, calculate, cache and return the inverse of the matrix
## Input: Cached Matrix, Returns: Inverse of Cached Matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
