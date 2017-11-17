## Put comments here that give an overall description of what your
## functions do


Below we call the function with a matrix, compute the inverse, retrieve
the inverse from the cache list, change the call matrix to the inverse,
compute the inverse on that and return the original function.

## Write a short comment describing this function

makeCacheMatrix creates a list containing a function to set/get value
of matrix and set/get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invr <<- inverse
    getinverse <- function() invr
    list(set=set, get=get,
    setinverse=setinverse,
    getinverse = getinverse)

}


## Write a short comment describing this function

The below fn returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

            invr <= x$getinverse()
            if(!is.null(invr)) {
            message("Getting Cached Data:-")
            return(invr)
            }
            matrx <- x$get()
            invr <- solve(matrx, ...)
            x$setinverse(invr)
            invr

}
