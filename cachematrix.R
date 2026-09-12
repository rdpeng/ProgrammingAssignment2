## These two functions work together to cache the inverse of a matrix,
## so that if the inverse has already been calculated for the current
## matrix, it can be retrieved from the cache instead of recomputed.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse. It returns a list of functions to set and get the
## value of the matrix, and to set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), it retrieves the inverse from
## the cache instead of recomputing it.

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
