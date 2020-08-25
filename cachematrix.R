# Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly 
#(there are also alternatives to matrix inversion that we will not discuss 
#here). Your assignment is to write a pair of functions that cache the inverse 
#of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(solve) invm <<- solve
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}

## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.
cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvm(invm)
    invm
}

M <- matrix(c(5, 2, 5, 3, -1, 3, -1, 4, 4), nrow=3, ncol=3)

