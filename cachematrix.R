##These functions calculate the inverse of a matrix and store it, then recall it as needed


##makeCacheMatrix saves the results of cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSlove sees if the inverse is calculated and, if not, calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachemean <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

}
