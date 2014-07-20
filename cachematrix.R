## My functions can calculate and cache the inverse of 
## a matrix, which can save considerable time when meet
## the same matrix again. Steps to use my functions:
## 1. Run "y <- makeCacheMatrix(x)". x is the invertible 
##      matrix of which you want to calculate inverse;
## 2. Run "cacheSolve(y)". y is the vector you got after
##      the first step.
## The output of my functions:
## 1. If the matrix is already cached, its inverse will 
##      be read from the memory and printed directly 
##      with the message "getting cached data"; 
## 2. Otherwise, the inverse will be calculated, cached 
##      and then printed.

## makeCacheMatrix:
## Return the special vector of four functions:
## 1. set: set the value of the vector
## 2. get: get the value of the vector
## 3. setinverse: set the value of the inverse
## 4. getinverse: get the value of the inverse
## Through these four functions, the value of the vector
## and the value of the inverse can be cached in another
## environment by the two "set" functions and can be read
## by the two "get" functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y ## All the "<<-" operators are used 
                        ## to cache the value in another 
                        ## environment.
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve:
## First check if the inverse is already cached. Then
## 1. Cached: write a message and return the inverse;
## 2. Not cached: calculate, cache and print the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { ##Check whether cached
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
