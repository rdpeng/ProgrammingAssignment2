## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) v <<-inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       v <- x$getinverse()
    if (!is.null(v)) {
        message("getting inverse matrix")
        return(v)
    } else {
        v <- solve(x$get())
        x$setinverse(v)
        return(v)
    }
}
