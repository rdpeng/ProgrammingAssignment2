## This module defines a class with several functions, intended to store a
## matrix with a possible cached inverse. The second function in the module
## allows for simple retrieval of the object's inverse by retrieving a cached
## inverse or computing a new one if one does not exist.

## makeCacheMatrix creates an object with a few functions as attributes.
## These functions allow the user to set or to get its value (which should be
## a matrix), it check for it. setinverse and getinverse allow setting and
## getting its inverse, so that it can be Cached in it after solving and
## retrieved upon ocmmand.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                if (!is.matrix(y)) {
                        warning("The data you've entered is not a matrix'")
                }
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                inv <<- inverse
                if (!is.matrix(inverse)) {
                        warning("The data you've entered is not a solved matrix'")
                }
        }
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve allows one to get the inverse of a makeCacheMatrix class object.
## If the inverse of the matrix was already solved and cached in the object it
## is retrieved and returned, otherwise the inverse is computed, cached in the
## object and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dataset <- x$get()
        inv <- solve(dataset, ...)
        x$setinverse(inv)
        inv
}