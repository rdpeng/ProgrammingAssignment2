## This function creates an object which caches the inverse, which is then retrievable.
## We assume below that all matrices are retrievable.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## function changing what is stored
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x    ## function returning 'x' from main function
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function solves the inverse of the matrix that was cached above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("retrieving...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}