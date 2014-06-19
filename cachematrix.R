## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x << - y
        m <<- NULL
        }
        get <- function() x
                setInverse <- function(solve) {
                m <<- solve
        }
        getInverse <- function() {
                if (is.null(m)) {
                m <<- solve(x)
                }
        return(m)
        }
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
