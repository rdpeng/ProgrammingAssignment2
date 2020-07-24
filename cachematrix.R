## Purpose:  Cache the inverse of a matrix
## Date: July 24, 2020
## By: Jennifer Ng

## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that stores
## the matrix and caches its inverse.

## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        
        ## initialize inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        set_inverse <- function(inverse) 
                inv <<- inverse
        get_inverse <- function() inv
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## The second function computes the inverse of the matrix created by makeCacheMatrix.
## When the inverse has already been calculated, the inverse will be retrieved from the cache.
## Otherwise it will be computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        inv <- x$get_inverse()
        if (!is.null(inv)) {
                message("using cached data")
                return(inv)
        }
        
        matrix1 <- x$get()
        inv <- solve(matrix1, ...)
        x$set_inverse(inv)
        inv
}
