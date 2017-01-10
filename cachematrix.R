## This code will cache the inverse of a matrix so we don't have to compute it every time.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Takes a matrix as it's input and returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve inverts the matrix created with the above function. It first checks to 
## see if the inverse has already been calculated (and the matrix has not changed). 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets it in the cache via 
## the `setinverse` function.

cacheSolve <- function(x, ...) {
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
