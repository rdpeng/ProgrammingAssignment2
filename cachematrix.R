## This function creates a special "matrix" object that can cache its inverse.
## Returns a list of functions to:
## 1. set the matrix, 2. get the matrix, 3. set the inverse, and 4. get the inverse
makeCacheMatrix <- function(m = matrix()) {
        inverted <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(m.inv) inverted <<- m.inv
        getinv <- function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## get the stored inverse of the matrix
        inverse <- x$getinv()
        if(!is.null(inverse)) { ## non-null value signifies valid cache
                message("getting cached data")
                return(inverse)
        }
        ## otherwise compute the inverse and store in cache
        ## before returning the result
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse ## return the inverse
}
