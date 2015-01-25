## These functions can cache matrix inverses in order to reduce the time needed to compute
## the inverses of previously used matrices. This is especially useful when the inverses of 
## these matrices are computed repeatedly.

## This function creates a matrix that allows the user to cache its inverse through a list;
## this list contains four different functions.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) n <<- solve  ## for storing inverse for specific matrix
        getinverse <- function() n  ## for retrieving cached inverse for specific matrix
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function checks if the inverse is calculated.
## If so, this function retrieves the inverse from the cache;
## otherwise, it calculates and stores the inverse.

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {  
                message("getting cached data")
                return(n)
        }
        data <- x$get()  ## skipped if inverse is previously calculated
        n <- solve(data, ...)  ## solves inverse of specific matrix
        x$setinverse(n)  ## stores inverse to cache
        n
}
