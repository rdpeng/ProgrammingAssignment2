## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() would create a matrix and cache it's inverse via a setInverse() API
## cacheSolve() would compute an inverse of a matrix if not done before (cached)

## makeCacheMatrix() would create a matrix and cache it's inverse via a setInverse() API
## Would return a list of getter and setter methods for matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(matrixInv) matrixInverse <<- matrixInv
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() would compute an inverse of a matrix if not done before (cached)
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    matInv <- x$getInverse()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    data <- x$get()
    matInv <- solve(data, ...)
    x$setInverse(matInv)
    matInv
}
