## A pair of functions that store a matrix, cache it's inverse, and if necessary,
## calculate the inverse of the matrix

## 'makeCacheMatrix' is a function that creates an object that stores a matrix
## and caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    # Set matrix to NULL
    m <- NULL
    # Create function to set matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Create function to get matrix
    get <- function() x
    # Create function to set the inverse of the matrix
    setInvM <- function(invM) m <<- invM
    # Create function to get the inverse of the matrix
    getInvM <- function() m
    # Create makeCacheMatrix list object containing all functions  
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}

## 'cacheSolve' is a function that calculates the inverse of the matrix
## created with 'makeCacheMatrix'. However it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets the inverse matrix in the cache via the 'setInvM' function 

cacheSolve <- function(x, ...) {
    # retrieve matrix from object passed in argument
    m <- x$getInvM()
    # Check whether result is NULL. If not, return message "getting cached data"
    # and return stored matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Get stored matrix from object passed to argument
    data <- x$get()
    # Calculate inverse of matrix and store it in object 'm'
    m <- solve(data, ...)
    # Store the inverse of the matrix 'm' in the 'makeCacheMatrix' object
    x$setInvM(m)
    # print matrix
    m
}