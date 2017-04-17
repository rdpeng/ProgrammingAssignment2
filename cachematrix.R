## Caching the inverse of a matrix (assuming input matrix is invertible)
## Derek Funk 4/16/2017

## This function creates a list that has functions to set the values of the input matrix,
## retrieve them, set the values of the inverse matrix, and retrieve those.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function checks whether the inverse of the input matrix has already been calculated.
## If so, it will return the inverse from the cache. Otherwise, it will solve as normal.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}