## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  cacheinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       cacheinverse = cacheinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x=matrix, ...) {
     m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$cacheinverse(m)
    m
  }
  ## Return a matrix that is the inverse of 'x'
}
