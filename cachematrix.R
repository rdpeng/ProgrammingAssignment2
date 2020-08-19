## Put comments here that give an overall description of what your
## functions do

## first function which creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse)inv<<-inverse
        getinv <- function() {inv}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## second function computes inverse of the special matrix
## if inverse is already calculated, it will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {         
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv    ## Return a matrix that is the inverse of 'x'
}
