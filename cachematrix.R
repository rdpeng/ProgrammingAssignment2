## These functions calculate the inverse of a matrix and saves it to the cache

makeCacheMatrix <- function(x = matrix()) {
        ## This is a matrix, x
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function(inverse) m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

