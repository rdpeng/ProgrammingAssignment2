## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a matrix object that caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) k <<- solve
        getsolve <- function() k
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

## This function calculate the inverse of the matrix returned by the makeCacheMatrix.
## if the inverse has already been calculated,without changes in the matrix, cacheSolve 
## returne the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getsolve()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setsolve(k)
        k
}
