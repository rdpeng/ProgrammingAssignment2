## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverese <- function() inv
list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinv(i)
  i
}
