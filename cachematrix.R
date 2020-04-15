## This pair of functions cash the inverse matrix, so that the inverse matrix is calculated 
## only the first time it is needed. 

## This function createes the following list of functions:
## set() to set the value of the matrix
## get() returnss the value of the matrix
## setInverse() sets the inverse matrix
## getInverse() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(i) {
        inv <<- i
    }
    getInverse <- function() {
        inv
    }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse matrix. If the inverse has not been calculated
## before, the function calculates and chashes it, otherwise it returns
## the cashed value.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (is.null(inv)) {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    } else {
        message("getting cashed data")
    }
    inv
}


