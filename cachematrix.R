## Function 01 and Function 02 aim at increasing the efficacy of 
## our for matrix inversion computations by creating and caching 
## its inverse instead of doing it repeatedly for the same dataset.

## Function 01 = "makeCacheMatrix"
## This function creates a an object (matrix class) 
## which will be used in Function 02 as the inverse cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) m <<- solve
        get_inv <- function() m
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
        
}


## Function 02 = "cacheSolve"
## This function computes the inverse of the object created
## in Function 01 ("makeCacheMatrix"):
# 1. It first checks if the inversion has already been computed.
# If so, it returns the inverse from the cache. 
# 2. Otherwise, it runs the "solve" function on the data and sets 
# stores the result as cache via the "set_inv" function.


cacheSolve <- function(x, ...) {
        m <- x$get_inv()
        if(!is.null(m)) {
                message("Haha, I've already done that! 
                        => getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        m
}
