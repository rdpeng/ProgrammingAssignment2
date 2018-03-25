x<-matrix(1:6,nrow=3,ncol=2)
 m<-makeCacheMatrix(x)
 makeCacheMatrix <- function(x = matrix()) {
 setmatrix <- function(matrix) m <<- matrix
 getmatrix <- function() m
 inv <- NULL
 set <- function(y) {
 x <<- y
 inv <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv
 list(set = set, get = get,
 setinverse = setinverse,
 getinverse = getinverse)
 }
 cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
 if(!is.null(inv)) {
 message("Getting cached matrix")
  return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
  }
