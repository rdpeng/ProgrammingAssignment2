## Put comments here that give an overall description of what your
## functions do

## Caching the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    set <- function(y) {
        x <<- y
        inv <<- matrix()
    }
    get <- function() x
    setinv <- function(matrix_inv) inv <<- matrix_inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## check whether a inverted matrix is cached, if not, cache it, otherwise read from the cached version.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.na(inv[1,1])) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
