## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## This assignment is to write a pair of functions that cache the inverse of 
## a matrix.

## This function of makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invers <<- solve
        getinverse <- function() invers
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      

}


## This function of cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        invers <- x$getinvers()
        if(!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }
        data <- x$get()
        invers <- solve(data, ...)
        x$setinverse(invers)
        invers
        ## Return a matrix that is the inverse of 'x'
}
