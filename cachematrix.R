## Put comments here that give an overall description of what your
## functions do
## Compute inverse of matrix
## Write a short comment describing this function

## cache the matrix - Return vector of getter and setter functions for the 
## matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  sinverse <- function(inverse) m <<- inverse
  ginverse <- function() m
  list(set = set, get = get, sinverse = sinverse, ginverse = ginverse)
}


## Write a short comment describing this function

## solve the matrix to get the inverse of the matrix/retrieve a cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}
