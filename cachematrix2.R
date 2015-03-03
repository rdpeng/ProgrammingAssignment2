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

        ## Return a matrix that is the inverse of 'x'

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
