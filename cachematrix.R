## Functions to handle inverse matrix computation
## that uses a cached variable to avoid
## repeating costly computations


## Creates a special matrix object as
## a list of setter/getter functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## Function to calculate inverse of special matrix
## created by makeCacheMatrix, but only calculates
## inverse if value is not already in cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Retrieving inverse matrix from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) # solve function calculates inverse matrix
    x$setinverse(i)
    i
}