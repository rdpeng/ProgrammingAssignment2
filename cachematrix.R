## Purpose of the two functions below is to cache the inverse of a given matrix and return the inverse either directly from the cache or from the 
## function that computes the inverse of the matrix.

## Function makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Function cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
