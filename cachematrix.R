## Cache Inverse of a given Matrix using pair of functions
## makeCacheMatrix and cachesolve

## function that is capable in caching inverse matrix.the following steps includes
## setting gathered inverse value of the matrix, gathering matrix value.
makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
}
	get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## function that computes cache of given inverse matrix created by the above function 
## makeCacheMatrix. the funtion of the setup returns the iverse of matrix 'x' as well as getting the object matrix. 
cacheSolve <- function(x, ...) {
        invrs <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setInverse(invrs)
        inv
}
