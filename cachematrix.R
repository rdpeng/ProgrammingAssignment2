# makeChacheMatrix function creates a "special" matrix, that allows set its value, its inverse 
# value, and also to get those values

# cacheSolve functions takes as an argument a "special" matrix created by makeCacheMatrix function
# and checks whether the inversed value for it was calculated and cached: if it was, it returns it,
# otherwise it calculates it, caches it in a "special" matrix and returns it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversed <- function(inversed) m <<- inversed
    getinversed <- function() m
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinversed()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversed(m)
    m
}
