## makeCacheMatrix and cacheSolve functions combined provide a 
## feature to create square invertible matrix that caches its inverse 
## for time efficient repeated invocation of solve()

## makeCacheMatrix creates a special matrix that  
## stores the matrix along with its inverse and provides
## getters and setters to access those.
## The inverse is set to NULL during the construction for efficiency.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(m.inv) inv <<- m.inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## calculates the inverse of a matrix
## Assumes that x is an invertible square matrix
## First invocation caches the inverse returned by the solve(...) and the
## subsequent call returns it straight from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
