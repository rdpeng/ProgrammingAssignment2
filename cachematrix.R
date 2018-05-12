## Cache the inverse of a matrix

## This function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(b) {
                x <<- b
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of the matrix set in makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## the cachesolve retrieves the inverse from the cache, otherwise it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
