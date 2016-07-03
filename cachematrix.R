## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmat <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmat <- function() x
        setmatInverse <- function(inverse) i <<- inverse
        getmatInverse <- function() i
        list(
                setmat = setmat,
                getmat = getmat,
                setmatInverse = setmatInverse,
                getmatInverse = getmatInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmatInverse()
        if (!is.null(inv)) {
                message("Receiving Cache")
                return(inv)
        }
        mat <- x$getmat()
        inv <- solve(mat, ...)
        x$setmatInverse(inv)
        inv
}
