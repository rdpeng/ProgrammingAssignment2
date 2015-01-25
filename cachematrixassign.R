#Fork a new file
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    x <- matrix(rnorm(64), ncol = 8, nrow = 8)
}

##cachesolve

cacheSolve <- function(x, ...) {
    solve(x)
    x 
}
##cacheinversion

cacheInversion <- function(x, ...) {
    start_time <- Sys.time()
    m <- x$getinversion()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    end_time <- Sys.time()
    time_duration <- end_time - start_time
    sprintf("the time to calculate was %s", time_duration)
    
    m
}

makeMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) m <<- solve
    getinversion <- function() m
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}
