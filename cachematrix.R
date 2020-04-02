## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix - initialize a matrix wrapper containing its cached inverse
# cacheSolve - return the cached inverse or compute and cache it if not computed

## Write a short comment describing this function
# This function defines a matrix wrapper containing cache for its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(m){
                x <<- m ; inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This function returns the cached inverse or compute and cache it.
cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if(is.null(inv)) {
                message("Caching matrix inverse..")
                x$setinv(solve(x$get(),...))
                inv = x$getinv()
        } else {
                message("Matrix inverse already stored!")
        }
        ## Return a matrix that is the inverse of 'x'
        inv
}
