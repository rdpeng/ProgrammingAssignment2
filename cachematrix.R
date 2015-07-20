# makeCacheMatrix() creates special matrix object that can cache its inverse.
# Takes a matrix as input, outputs a list. Be sure to store the output to another
# variable. This will be the input for cacheInverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Retrieves inverse of makeCacheMatrix from cache if already created. Otherwise
## computes and caches inverse of matrix. Returns the inverted matrix

cacheInverse <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

