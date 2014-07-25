## The functions in this program create a matrix that can cache its inverse,
## compute the inverse of the matrix, and retrieve the inverse from the cache

## The first function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
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


## The second function computes the inverse of the special matrix returned
## by "makeCacheMatrix" above.
## If the inverse has already been calculated, then this function retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Check to see if the inverse has already been calculated
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
