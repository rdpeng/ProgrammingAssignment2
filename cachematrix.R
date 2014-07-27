## The following functions are based on the 'makeVector' and 'cachemean' from the examples in the Programming Assignment 2 of the R Programming Course (Coursera, July 2014).

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL # 'im' for inverse matrix
        set <- function(y) {
                x <<- y
                im <<- NULL 
                # cache im object as NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve 
        # cache the inverse matrix in the im object
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        # if the function has already been run the im variable will not be
        # NULL
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        # if being run for the first time, this will cause the im object to 
        # be cached with the solved inverse matrix.
        im
}