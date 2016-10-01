## Creates the inverse of a matrix and provides a cached version, if available
## Created By: Dennis Salguero, 2016

## Creates the inverse of a provided matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If available, returns a cached matrix. If not available, it finds the answer and caches it
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  matrixdata <- x$get()
  m <- solve(matrixdata,...)
  x$setinverse(m)
  m
}
