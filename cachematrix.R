## Caching the Inverse of the Matrix
## get the value of the inverse
## stores a matrix and caches its inverse.

## Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## calculates the inverse of the special "matrix"
## it should retrieve the inverse from the cache,If the inverse has been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()  
    m <- solve(data, ...)
    x$setInvmatrix(m)
    m
}
