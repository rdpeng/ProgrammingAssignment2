## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i1 <- NULL
        set <- function(y) {
                x <<- y
                i1 <<-NULL
        }
        get <- function() x
        setinverse <- function(inv) i1 <<- inv
        getinverse <- function() i1
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i2 <- x$getinverse()
        if(!is.null(i2)) {
                message("getting cached matrix")
                return(i2)
        }
        data <- x$get()
        i2 <- solve(data, ...)
        x$setinverse(i2)
        i2
}
