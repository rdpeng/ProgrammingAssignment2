## set of 2 functions for creating a "matrix" object that can cach its inverse and calculating and returning it

## creates a soecial object that can hold a matrix and its inverse (assuming its invertible)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculates and returns the inverse of a matrix returned by the above functions

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
