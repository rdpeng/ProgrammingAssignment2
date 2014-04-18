## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    # initialize the stored inverseerse value to NULL
    inverse <- NULL

    # to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL   # since the matrix changed
    }
    # to get the value of the matrix
    get <- function() x
    # to set the inverseerse
    setinverse <- function(inverse_) inverse <<- inverse_
    # to get the inverseerse
    getinverse <- function() inverse

    # return a list of all the above functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## The following function calculates the inverseerse of the special 
## "matrix" created with the above function. However, it first 
## checks to see if the inverseerse has already been calculated. 
## If so, it gets the inverseerse from the cache and skips the 
## computation. Otherwise, it calculates the inverseerse of the 
## matrix and sets the value of the inverseerse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
    # check if the inverseerse is already cached
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # not cached, so we get the matrix into data
    data <- x$get()
    # and compute the inverseerse
    inverse <- solve(data, ...)
    # then cache the inverseerse
    x$setinverse(inverse)
    # and return it as well
    inverse
}
