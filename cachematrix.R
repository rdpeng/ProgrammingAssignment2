## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. This pair of functions caches the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        matrix <<- newMatrix
        cachedInverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(inverse) cachedInverse <<- inverse
    getinverse <- function() cachedInverse
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {
    inverse <- cacheMatrix$getinverse()
    if (!is.null(inverse)) {
        message("returning cached inverse")
        return(inverse)
    }
    matrix <- cacheMatrix$get()
    inverse <- solve(matrix, ...)
    cacheMatrix$setinverse(inverse)
    ## Return a matrix that is the inverse of 'cacheMatrix'.
    message("returning newly calculated inverse")
    inverse
}

## Unit test for the functions in this file.
unitTestCacheMatrix <- function() {
    m <- makeCacheMatrix(matrix(c(1, 0, 0, 1), 2, 2))
    print(cacheSolve(m))
    print(cacheSolve(m))
    print(cacheSolve(m))
    m$set(matrix(c(2, 0, 0, 4), 2, 2))
    print(cacheSolve(m))
    print(cacheSolve(m))
    print(cacheSolve(m))
    m$set(matrix(c(1, 2, 3, 3, 2, 1, 1, 2, 1), 3, 3))
    print(cacheSolve(m))
    print(cacheSolve(m))
    print(cacheSolve(m))
}
