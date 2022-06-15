## Caching the Inverse of a Matrix


## makeCacheMatrix function creates a list containing a function to set the value of the vector, 
## get the value of the vector, set the value of the mean, and get the value of the mean.

        makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
  }
        get <- function() x
        setInverse <- function() inv <<- solve(x)
        getInverse <- function() inv
        
        list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

