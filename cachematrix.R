## R programming assignment Deadline : 9-27-2015
makeCacheMatrix <- function(x = matrix()) { ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
    m <- NULL ## m is set to null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse ## solve function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
    getInverse <- function() m
    list(set = set,    ## to make a List of function.
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

cacheSolve <- function(x, ...) { ## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$setInverse(m)
    m
}