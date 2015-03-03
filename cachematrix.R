e118d5b8cce0f5ac78c135147e1681f67053aa6a

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

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    } else {
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        return(m)
    }

}
