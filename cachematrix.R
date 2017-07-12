## makeCacheMatrix creates a special matrix by first reading an input matrix x and caches it. Sets default to NULL 

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invert <<- inverse
    getInverse <- function() invert
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Return a matrix that is the inverse of input matrix x by first checking if matrix already calculated

cacheSolve <- function(x, ...) {
    invert <- x$getInverse()
## If inverse matrix has previously been computed and the matrix has not changed, then the inverse from the cache is retrieved instead.
    if (!is.null(invert)) {
        message("retrieving cache")
        return(invert)
    }
## If inverse matrix wasn't previously computed, calculates and sets inverse matrix, and outputs inverse matrix
    data <- x$get()
    invert <- solve(data, ...)
    x$setInverse(invert)
    invert
}
