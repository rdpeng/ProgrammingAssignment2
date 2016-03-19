## Caching the inverse of a matrix
## The functions below are designed for creating an object to store
## the matrix and cache its inverse

## Creating an object to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    	x <<- y
    	inv <<- NULL
    }
    get <- function () x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list (set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}
   


## Computing the inverse of the matrix from the above function
## Retrieving the inverse from the cache, if the calculated inverse
## has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse ()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return (inv)
        }
        mat <- x$get()
        inv <- solve (mat, ...)
        x$setInverse(inv)
        inv       
}

