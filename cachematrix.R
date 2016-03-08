##Caching the Inverse of a Matrix

## 'makeCacheMatrx' creates a special 'matrix' object that can cache its inverse. 
## It creates a list containing a function to set and get the value of the matrix
## and to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## 'cacheSolve' computes the inverse of the special "matrix" returned by 
## 'makeCacheMatrix'. If the inverse has already been calculated (and 
## the matrix has not changed), then 'cacheSolve' retrieves the inverse 
## from the cache. It returns a matrix that is the inverse of 'x'. 

cacheSolve <- function(x=matrix(), ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}