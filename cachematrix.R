## The function makeCacheMatrix creates and returns an object based on a 
## invertible
## matrix (this is an assumption and is not verified) and caches its
## inverse after the first call to getinverse.
## get, set, setinverse and getinverse can be used to get or set the 
## matrix or its inverse.

## The function cacheSolve takes an object created by makeCacheMatrix and
## computes the inverse and caches it
## if this has not already been done by a previous
## call. 
## cacheSolve returns the inversed matrix.

## Write a short comment describing this function
makeCacheMatrix <- function(m = matrix()) {
        ## m is an invertible matrix
        ## minverse is the inverse and is reset
        minverse <- NULL
        ## setter for the matrix that also reset the inverse
        set <- function(newm) {
                m <<- newm
                minverse <<- NULL
        }
        ## getter for the matrix
        get <- function() m
        ## setter for the inverse
        setinverse <- function(newinverse) minverse <<- newinverse
        ## getter for the inverse
        getinverse <- function() minverse
        ## name the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        ## m is an invertible matrix
        ## get the inverse stored in m
        minverse <- m$getinverse()
        ## if it is not null, then it is already computed, so return it
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        ## minverse is NULL, so compute it, store it and return it.
        data <- m$get()
        minverse <- solve(data, ...)
        m$setinverse(minverse)
        minverse
}


