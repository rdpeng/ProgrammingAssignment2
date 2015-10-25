## This file has functions to solve Coursera R programming Assignment#2.
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## This assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    myInverse <- NULL
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    get <- function() x
    mySetInverse <- function(inverse) myInverse <<- inverse
    myGetInverse <- function() myInverse
    list(set = set,
         get = get,
         mySetInverse = mySetInverse,
         myGetInverse = myGetInverse)
}
    
## 2.cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    myInverse <- x$myGetInverse()
    if (!is.null(myInverse)) {
        print("From Cache:")
        return(myInverse)
    }
    myMatrix <- x$get()
    myInverse <- solve(myMatrix, ...)
    x$mySetInverse(myInverse)
    myInverse
}

