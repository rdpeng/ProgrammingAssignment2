## The cacheSolve() function will check to see if 
## the inverse of a matrix, X, exists in the parent environment
## by calling makeCacheMatrix().  If it does not exist, then it
## will calculate it and return the inverse.

## The makeCacheMatrix() function will store the inverse of a
## matrix into its unique environment to be used later.

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


## CacheSolve attempts to get the inverse of x and store it into
## a free variable m.  Since getinverse() does not exisit in the
## function it checks the calling functions directory to see if m is stored there.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
