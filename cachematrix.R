## This function creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the matrix returned by makeCacheMatrix,
## returning it from cache if it is already calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
			message("getting cached data")
          return(m)
        }
        theData <- x$get()
        m <- solve(theData, ...)
        x$setinv(m)
        m
}
