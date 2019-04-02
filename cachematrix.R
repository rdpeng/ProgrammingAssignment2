## This function creates an object for storing an matrix in the cache
makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invert <<- inverse
        getInverse <- function() invert
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse of the passed matrix, by means of the makeMatrixCache function.
## This function checks if the matrix is reversed, if it recovers the matrix already inverted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getInverse()
        if (!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        matrix <- x$get()
        invert <- solve(matrix, ...)
        x$setInverse(invert)
        invert

