## makeCacheMatrix
##
## Create a special "matrix", which functions to:
## - Set the value of the matrix
## - Get the value of the matrix
## - Set the value of the inverse
## - Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inversed) inverse <<- inversed
    getInverse <- function() inverse
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve
##
## Compute the inverse of the given (invertible) special matrix. 
## If the inverse matrix was calculated before, it returns the cached result.
cacheSolve <- function(x, ...) {
    # Get solved matrix from cache
    inverse <- x$getInverse()
    
    # If matrix doesn't exist in cache, solve it and put into cache
    if (is.null(inverse)) {
        # Calculate inversed matrix
        inverse <- solve(x$get(), ...)
        
        # Cache the result
        x$setInverse(inverse)
    } else {
        message("Getting data from cache")
    }
    
    # Return inversed matrix
    inverse
}