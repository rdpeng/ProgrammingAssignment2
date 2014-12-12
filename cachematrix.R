## This function makes the solve(matrix) function more efficient by caching inverts 
## for ulterior uses. 

## This function returns a list of functions to handle the caching process. 
## $set(y) : sets a new matrix y and clears the cache
## $get() : returns the matrix stored in the function
## $setsolve(solve) : sets the cache to solve
## $getsolve() : returns the cached invert, or NULL if it has not been calculated

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


## This function checks for cached inverted matrix, else calculates it, and returns the invert

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
