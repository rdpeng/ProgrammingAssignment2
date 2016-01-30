## --------------------------------------------------------------------------
## R Programming - week 3 - assignment 2
## --------------------------------------------------------------------------
## In this file there are two functions that implement a simple chache 
## mechanism. The first one creates a special matrix object that can 
## cache its inverse. The second one, instead, computes the inverse matrix of 
## a matrix  using the previous function and caches the results for a better
## performance.
## --------------------------------------------------------------------------


## --------------------------------------------------------------------------
## The function "makeCacheMatrix" is a sort of constructor that 
## create a special list containing the input Matrix (stored in its environment)    
## and a list of functions that operate on it.
## the functions are:
## "set"  provides to setting the environment variable "x" containing the matrix
## "get"  provides to get the matrix "x" contained in environment variable
## "setinvm"  provides to setting the variable "invm" containing inverse matrix
## "getinvm"  provides to get the inverse matrix "invm" contained in environment

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinvm <- function(invx) ix <<- invx
    getinvm <- function() ix
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
    
}

## --------------------------------------------------------------------------

## The function "cacheSolve" calculates the inverse matrix of the special
## matrix object created with previous function. It performs the following 
## elaborative steps:
## 1) first checks if the inverse matrix has already been calculate
## 2) if calculated get the results from cache and return
## 3) Otherwise it calculates the inverse matrix and stores the result in cache
##
cacheSolve <- function(x, ...) {
    invx <- x$getinvm()
    if (!is.null(invx)) {
        message("getting cached inverse matrix")
        return(invx)  
    }
    matx <- x$get()
    invx <- solve(matx,...)
    x$setinvm(invx)
    invx
}
