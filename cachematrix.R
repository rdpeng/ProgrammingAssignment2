## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## This Funchtion creates a Matrix with a function to cache the inverse of the
## matrix so the inverse only has to be called once

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list( set = set, get = get,
              setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
##
## this function will ether caculate the inverse of a matrix and store it 
## or retern a previously stored inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        message("calculating inverse")
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
