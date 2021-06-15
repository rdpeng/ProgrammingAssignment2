#done
## Function creates a special "vector", which is really a list containing a function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setM <- function(solve)
    m <<- solve
  getM <- function()
    m
}

## Function creates a special "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getM()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setM(m)
  m
  
}