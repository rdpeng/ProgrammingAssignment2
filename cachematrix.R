## This function will cache the inverse of a matrix:
## Matrix inversion is a heavy task using a lot of memory and space

## makecachematrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list (set=set,get=get, setInverse = setInverse, getInverse = getInverse)
}


## Below function calculates the inverse of the special matrix created above and returns the cache

cacheSolve <- function(x, ...) {
            inv <- x$getInverse()
            if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
            }
            mat <- x$get()
            inv <- solve(mat,...)
            x$setInverse(inv)
            inv
}
