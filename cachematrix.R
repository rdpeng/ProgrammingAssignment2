## Put comments here that give an overall description of what your
## functions do
## This R script contains 2 functions which allow for the creation of a matrix
## in which it's inverse can be cached to improve performance
## A function which contains functions for the operations required
## to create, store, retrieve and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculate and return the inverse of a matrix.  The result
## is cached and returned from the cache if it already exists.
cacheSolve <- function(x, ...) {
m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
} 	 	 
