## This R file contains 2 functions. One with 4 functions to set and retrieve matrix. Other method to cache and return cached matrix which will inturn call the get and set functions.

## Following function sets the inversed matrix and provides functions for set, get setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(x) invmat <<- x
    getinverse <- function() invmat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function inverts the matrix using solve function and uses functions within the makeCacheMatrix. Returns cached matrix if available
cacheSolve <- function(x, ...) {
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data, ...)
    x$setinverse(invmat)
    invmat
}
