## These functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            X <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the function above
## If the inverse has already been calculated, the this function will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInverse(inv)
        inv
}