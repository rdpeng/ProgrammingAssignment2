makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setval <- function(mean) m <<- mean
        getval <- function() m
        list(set = set, get = get,
             setval = setval,
             getval = getval)
}
cacheSolve <- function(x, ...) {
        m <- x$getval()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setval(m)
        m
}
