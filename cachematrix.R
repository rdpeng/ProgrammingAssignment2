## These functions attempt to reduce computational
## burden by caching the inverse of a squared matrix

## Sets the matrix to be inverted and cached

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverted <- function(solve) m <<- solve
    getinverted <- function() m
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinverted()
    if(!it.NULL(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}
