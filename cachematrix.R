## Put comments here that give an overall description of what your
## functions do


## the function makeCacheMatrix does following:
## 1. set the matrix 	
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse1) inverse <<- inverse1
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## the function cachesolve returns inverse of matrix. It computes inverse if 
## already not done and returns inverse. But if inverse has already been 
## calculated , it directly return the inverse values.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    Inverse

    }