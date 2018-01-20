## Put comments here that give an overall description of what your
## functions do
      ## makeCatchnatrix function ccreate a matrix and catch its inverse and the second function maned catchSolve
      ## computes the inverse returned by makeCatchMatrix 

## Write a short comment describing this function

      ## This functoin creates a special matrix that catch its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, 
         get=get, 
         setinverse=setinverse,
         getinverse=getinverse)
}

## Write a short comment describing this function
## The function returns inverse of special matrix created by makeCatchMatrix
cacheSolve <- function(x, ...) {
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
