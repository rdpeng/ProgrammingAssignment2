## Put comments here that give an overall description of what your
## functions do

## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## Write a short comment describing this function

## This function creates a special "matrix" object
## that can cache its inverse.

makeVector <- function(x = matrix()) {
        inverse <- NULL
        set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return(inverse);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Write a short comment describing this function

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cachemean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
        data <- mtx$get()
        invserse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
}
