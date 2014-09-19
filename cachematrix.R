
##This function is built so that, once the inverse of a matrix is calculated, that value is stored to
##minimise on calculation time on future calls.

## makeCacheMatrix creates a list of functions to store and return a matrix and its cached inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}


## cacheSolve returns the inverse of a matrix stored in a makeCacheMatrix list. It will return the 
##Cached value of the inverse if it exists else it will calculate and cache the inverse before returning it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}