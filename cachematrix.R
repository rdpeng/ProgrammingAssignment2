## Two functions that cache the inverse of a matrix in order to cache potentially time-consuming computations


## Create a special object that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
        sol_m<- NULL
        set <- function(y) {
                x <<- y
                sol_m<<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sol_m <<- solve
        getsolve <- function() sol_m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        inverse <- x$getsolve()
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
