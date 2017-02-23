## This is an R script (containing two parts) to cache potentially time consuming computations.
## The purpose is to invert a matrix.
## The first part creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second part computes the inverse of the special matrix.  
## If the inverse has already been calculated and the matrix has not changed, then it
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matdata <- x$get()
        inv <- solve(matdata, ...)
        x$setInverse(inv)
        inv
}
