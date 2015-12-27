## The purpose of the following two functions are to efficiently utilise processing capacity
## when calculating the inverse of a matrix through cacheing.

## makeCacheMatrix sets and gets the matrix and inverse values 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function checks to see if m has a value, if m has a value this value is printed
## otherwise, the inverse of the matrix specified through the above function is calculated

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
