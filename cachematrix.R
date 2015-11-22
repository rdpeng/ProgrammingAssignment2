## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the inverse of the original matrix, x, from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## otherwise will calculate the inverse
        
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ## sets the value of the inverse of the cache via the setInverse function
        
        x$setInverse(inv)
        
        inv
}
