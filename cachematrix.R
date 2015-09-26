
## matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a specialmatrix object that can cache its inverse.

makeCacheMatrix <- function(z = matrix()) {
        inv <- NULL
        set <- function(y) {
                z <<- y
                inv <<- NULL
        }
        get <- function() z
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated  
## then it should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- z$getInverse()
        if (!is.null(inv)) {
                message("Cached data")
                return(inv)
        }
        mat <- z$get()
        inv <- solve(mat, ...)
        z$setInverse(inv)
        inv
}
