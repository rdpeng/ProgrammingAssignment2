## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) Inv <<- Inverse
        getInverse <- function() Inv 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function omputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
