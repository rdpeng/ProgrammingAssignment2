## This program creates a matix object and it caches its inverse value.
## If value is cached, it returns the value from the cache(memory).
## If its not cached, it calculates and returns the inverse value.
## It assumes that the matrix supplied is always invertible. It doesn't work if its not invertible.
##
## Usage:
## x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(x) --> calclulates invrese values
## cacheSolve(x) --> returns value from the cache
##


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getInverse()

        if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }

        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        return(inverse)
}
