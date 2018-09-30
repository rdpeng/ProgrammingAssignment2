# This first function will caches the inverse of a matrix
# while the second will caches the inverse of the same matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# This function calculate the inverse of the matrix created previously. If the inverse has been calculated then 
# it will be displayed from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
