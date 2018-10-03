## The functions below enable to cache the solution to a matrix inversion.
## Storing the inverted matrix in cache allows to access it without taking time 
## to recompute it. 

## makeCacheMatrix creates a list containing a function to set the 
## matrix, get the matrix, set the inverted matrix and get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## CacheSolve calculates the invert of the matrix created with the above function.
## It first checks if the inversion has already been computed. If so, it gets the
## inverted matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets its'  value in the cache via setsolve function

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