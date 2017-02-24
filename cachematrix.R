## cacheMatrix is a set of functions to cache results of a computationally
## expensive matrix inversion. It works by storing a previously computed
## inverted matrix and returns that if it was already computed. Otherwise,
## it computes and stores this. To facilitate this, we create a special lexical scoped
## wrapper that has a set, get, setinverse and getinverse functions.

## construct a special matrix, one that wraps the matrix with a cache, and the cache
## contains the previously computed inverse object.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## check that a previously solved inverse exists and return that.
## if it doesn't exist, solve for the inverse and store it away for
## future use

cacheSolve <- function(x, ...) {
    # get inverse from cache
    inv = x$getinverse()
    
    # if it exists, return that
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    # get the matrix, compute its inverse, save it in cache and
    # return the inverse
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
