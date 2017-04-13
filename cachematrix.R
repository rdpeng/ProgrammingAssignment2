## inverse of matrix and it's cache

## function to find the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
       	 set <- function(y) {
                x <<- y
                m <<- NULL
}

        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## this function will return the cache data if already solved 
##else it will solve the matrix to find its inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
