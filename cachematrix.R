## Create matrix caching it's inverse matrix
## Return value: CacheMatrix
## Example: M <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverted <- function(solve) inverted <<- solve
    getinverted <- function() inverted
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## Inverse CacheMatrix
## Return value: standard R matrix
## Example: N <- cacheSolve(M)

cacheSolve <- function(x, ...) {
    inverted <- x$getinverted()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverted(inverted)
    inverted
}
