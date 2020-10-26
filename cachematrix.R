## Put comments here that give an overall description of what your
## functions do

## In this function I created, we are fetching a mattrix and setting and getting its value, and then setting and getting the value of the inverse matrix. 

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


## Here we compute the inverse of the before computed matrix. If the inverse has already been calculated, use the cache inverse.

cacheSolve <- function(x, ...) { ## Get a matrix that is inverse of x
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
