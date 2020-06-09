## These functions serve to cache the inverse of matrices,
## which can be a costly computation.

## This function will cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function will call the inverse of the matrix if it has been cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
