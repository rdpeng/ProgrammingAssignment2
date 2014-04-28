## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## This function computes the inverse of the special "matrix"  returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting inverse matrix")
        return(inv)
    } else {
        inv <- solve(x$get())
        x$setinverse(inv)
        return(inv)
    }
}
