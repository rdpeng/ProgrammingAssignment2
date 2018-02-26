## makeCacheMatrix is a function that computes the inverse of the (matrix) entered within the function itself.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (y) {
        x<<-y
        i<<-NULL
    }
    get <- function() x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i
    list(set=set,
        get=get,
        setinverse= setinverse,
        getinverse= getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has been already calculated, then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null (i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
