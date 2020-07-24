## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() {
        inver <- ginv(x)
        inver%*%x
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}





## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cachesolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(m)
    inv
}
