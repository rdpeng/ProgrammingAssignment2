## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ivM <- NULL
    set <- function(y) {
        x <<- y
        ivM <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse)   ivM <<- inverse
    getInverse <- function() ivM
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ivM <- x$getInverse()
    if(!is.null(ivM)) {
        message("getting cached inverse of matrix")
        return(ivM)
    }
    data <- x$get()
    ivM <- solve(data, ...)
    x$setInverse(ivM)
    ivM
}
