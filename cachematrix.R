## This function creates a matrix that has the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function checks if the inverse has been computed in the cache
## If it has, it is returned. If not, it is calculated, cached and returned

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}