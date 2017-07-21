## The following two functions, makeCacheMatrix and cacheSolve, allow the user
## to compute the inverse of an invertiable matrix and cache the results


## The makeCacheMatrix creates a special "matrix" object that is able to cache 
## its inverse for future use

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

## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatix function. If the inverse of the special "matrix" has
## previously been computed by cacheSolve and the matrix has not changed,
## cacheSolve will retrieve the inverse from the cache

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