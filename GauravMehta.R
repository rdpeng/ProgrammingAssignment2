## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list from a matrix; this list will contain the necessary information to get/set the matrix and then the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix x, unless it's been already computed
## In that case, it will fetch it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message('Getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(x, ...)
    x$setinverse(m)
    m
}
