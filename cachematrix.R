## These functions calculate the inverse of a matrix and save it
## to the cache so the next time the matrix inverse is calculated
## the previous value is returned instead of repeating the calculation.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        setMatrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        getMatrix <- function() {
                x
        }
        setInverse <- function(inverse) {
                cache <<- inverse
        }
        getInverse <- function() {
                cache
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## the above makeCacheMatrix function. If the inverse has already been calculated
## and the matrix has not changed, the cacheSolve function will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
        if(!is.null(cache)) {
                message("Getting Cached Data")
                return(cache)
        }
        data <- x$getMatrix()
        cache <- solve(data)
        x$setInverse(cache)
        cache
}

