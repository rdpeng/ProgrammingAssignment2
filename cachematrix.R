## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    setMatrix <- function(y) {
        x <<- y
        im <<- NULL
    }
    getMatrix <- function() x
    setinverse <- function(inv) im <<- inv
    getinverse <- function() im
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if (!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
