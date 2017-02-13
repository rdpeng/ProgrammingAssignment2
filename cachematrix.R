## These two functions (makeCacheMatrix and cacheSolve) will cache the 
## inverse of a matrix so that it does not need to be calculated repeatedly.

## makeCacheMatrix - Creates an R object that stores (caches) the inversion of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) m <<- solve
        getinversion <- function() m
        list(set = set, get = get, setinversion = setinversion,
             getinversion = getinversion)
}


## Retrieves the cached value stored by makeCacheMatrix and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversion()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}
