##These functions works caching the inverse value of a matrix, saving time and processment when the value is needed again, in case of course, the matrix value hasn't changed since the last calculation.

##makeCacheMatrix (simillarly to makeVector) creates a list to set and get the value of the matrix and its inverse. Note how the cache variable(m) is set to NULL whenever the matrix value changes (when we need of course to calculate the inverse again).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


##cacheSolve checks if there is a inverse matrix already cached, by using the function getinverse. If it's NULL, we need to calculate it with the 'solve' function and then we cache it. If it isn't, it's because the matrix didn't change since the last inverse calculation, so the inverse will be the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
