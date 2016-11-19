## Two functions that allow chaching matrix inverse
## so it doesn't have to be re-calculated unless matrix
## data is changed.

## Creates a list, representing a matrix allowing inverse caching

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse...")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
