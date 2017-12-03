## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## Call this function first e.g. with 
## mx <- matrix(c(1,2,3,4), nrow=2, ncol = 2)
## cmx <- makeCacheMatrix(mx)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of a cached matrix
## Create a cached matrix first by calling 
## cmx <- makeCacheMatrix(mx)
## if the inverse was calculated before
## cacheInverse will return the results from the cached results
## in stead of recalculating the inverse

cacheInverse <- function(x, ...) {
        ## Return matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
