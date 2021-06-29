## These funcions will creat a cached inverse of a matrix

## This function will produce the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(s) {
                x <<- s
                i <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functiomn will create the overall outcome of the solution and will provide a cached inverse of a matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}
