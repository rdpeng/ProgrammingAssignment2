## These functions work together to first cache a very large matrix and then 
## solve for the invserse of the cached matrix if it hasn't already been done.

## The makeCacheMatrix function caches a matrix and returns a list of functions 
## to pass to cacheSolve to get and set the matrix, then get and set the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## set an empty parameter
        m <- NULL
        ## write the set() function within the environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## write the get() fuction within the environment
        get <- function() x
        ## write the setsolve() function within the environment
        setsolve <- function(solve) m <<- solve
        ## write the getsolve() function within the environment
        getsolve <- function() m
        ## and print the functions as a list
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function checks to see if the inverse of the matrix has been
## already computed, and if not, solves for the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
        ## looks for a solution in the cache
        m <- x$getsolve()
        ## if the solution is present, then...
        if(!is.null(m)) {
                ## get the solution...
                message("getting cached data")
                ## and print the result
                return(m)
        }
        ## if the solution is not present, then get the data...
        cacheData <- x$get()
        ## pass it to the solve() function...
        m <- solve(cacheData, ...)
        ## cache the solution...
        x$setsolve(m)
        ## and print the result
        return(m)
}