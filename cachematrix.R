## This function contains four sub-functions: set, get, setInv and getInv.
## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL 
        set <- function(y) {
                x <<- y
                xinv <<- NULL 
        }
        get <- function() x
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getInv() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data,...)
        x$setInv(m)
        m
}
        
