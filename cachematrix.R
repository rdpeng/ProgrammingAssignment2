## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # initialize my local variable
    inv <- NULL
    
    # set the matrix
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set / get the inverted matrix
    setInvert <- function(solve) inv <<- solve 
    getInvert <- function() inv
    
    # return the function descriptions in a list
    list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # check and return if already inverted
        inv <- x$getInvert()
        if(!is.null(inv))
        {
           message("getting cached data")
           return(inv)
        }
        # get the matrix and invert
        data <- x$get()
        inv <- solve(data, ...)
        
        # store and return the inverted matrix
        x$setInvert(inv)
        inv
}