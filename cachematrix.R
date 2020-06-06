## These functions are designed to cache the time consuming process of calculating
## the inverse of a square matrix.

## This function has an output that is a list of four different functions that 
## "set" the matrix, "get" the matrix, "set" the inverse, and "get" the inverse.

makeCacheMatrix <- function(x = matrix()) {
        mm <- NULL
        set <- function(y) {
                x <<- y
                mm <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) mm <<- solve
        getsolve <- function() mm
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calls upon our cached calculations to check for the inversed matrix,
## if it's not there it makes the necessary calculations

cacheSolve <- function(x, ...) {
        mm <- x$getsolve()
        if(!is.null(mm)) {
                message("getting cached data")
                return(mm)
        }
        data <- x$get()
        mm <- solve(data, ...)
        x$setsolve(mm)
        mm
        ## Return a matrix that is the inverse of 'x'
}
