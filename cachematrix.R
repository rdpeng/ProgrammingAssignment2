
## This function does caching the inverse of a Matrix

## set the value of the matrix, get the value of the matrix
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinverse <- function(solve) m <<- solve
    getinverse <- function () m
    list(set = set, get = get,
         setinverse =setinverse,
         getinverse =getinverse)

}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
