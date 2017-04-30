## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    y <- NULL
    set <- function(z) {
        x <<- z
        y <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) y <<- inverse
    getinv <- function() y
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    y <- x$getinv()
    if(!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    data <- x$get()
    y <- solve(data)
    x$setinv(y)
    y
}
