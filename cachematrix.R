@@ -1,15 +1,37 @@
## Put comments here that give an overall description of what your
## functions do
#Assignment: Caching the Inverse of a Matrix
#Your assignment is to write a pair of functions that cache the inverse of a 
#matrix.

## Write a short comment describing this function
#1. makeCacheMatrix: This function creates a special "matrix" object that can 
#cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#2. cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,diag(nrow(data)), ...)
    x$setinv(inv)
    inv
} 
