## These 2 functions work together to cache the inverse of a matrix if it has
##been precomputed before of compute it again and save it for futur use. 

## This function return a list of function that let us get or set the matrix, get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function caches the inverse of the matrix if it is already computed or computes the inverse and save it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
