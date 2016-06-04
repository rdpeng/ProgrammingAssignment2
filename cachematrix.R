## These functions first sets a matrix (x) and creates
## a list of functions to get and cache its inverse.
## The second function either pulls an existing 
## stored inverse and returns it, or calculates it 
## and then returns it.

## This function sets a matrix and returns a list 
## of functions to set and retrieve the inverse of
## a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function first attempts to find a cached
## inversion of matrix (x), and if it is NULL, it 
## calculates the inverse and returns it

cacheSolve <- function(x, ...) {
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
