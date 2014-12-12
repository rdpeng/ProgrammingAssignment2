## Matrix inversion is usually a costly computation, therefore better caching it rather than computing it repeatedly
## Below pair of functions that cache the inverse of a matrix
## Take advantage of the scoping rules of R and how they can be used to preserve state inside of an R object

## This function creates a special "matrix" object that can cache its inverse. Please note setting a new value resets cache

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cache <<- inverse
        getInverse <- function() cache
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
