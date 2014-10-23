## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The funcion makeCacheMatrix creates an object representing a matrix and which is 
## really a list with the functions to set the value of the matrix and get it, and also de value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(y) {
        mat <<- y
        inverse <<- NULL
    }
    get <- function() mat
    setInverse <- function(inver) inverse <<- inver
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function cacheSolve computes de inverse of the matrix represented by the object returned by the makeCacheMatrix function
## This function checks if the inverse has been previously calculated. In this case, returns the value from the cache. Otherwise 
## the inverse is calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
