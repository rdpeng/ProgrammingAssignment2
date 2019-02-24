## A pair of functions that store a matrix, cache it's inverse, and if necessary,
## calculate the inverse of the matrix

## 'makeCacheMatrix' is a function that creates an object that stores a matrix
## and caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvM <- function(invM) m <<- invM
    getInvM <- function() m
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


## 'cacheSolve' is a function that calculates the inverse of the matrix
## created with 'makeCacheMatrix'. However it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets the inverse matrix in the cache via the 'setInvM' function 

cacheSolve <- function(x, ...) {
    m <- x$getInvM()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvM(m)
    m
}