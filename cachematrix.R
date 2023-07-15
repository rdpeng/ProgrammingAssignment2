## The following functions can complete the matrix calculation

## makeCacheMatrix creates a unique matrix that consists of a function to set and get the elements of the matrix and set and get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve calculate the inverse of the special matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_invert <- x$get()
  inv <- solve(matrix_invert, ...)
  x$setinverse(inv)
  inv
}
