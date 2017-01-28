## The following functions will be able, not only to store the inverse of a function, but also to verify if this action has been already done and promote it, if necessary

## The function below is responsible for creating a matrix object, which is able to store its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Considering that the inverse of the matrix object was not computed by the previous function ("makeCacheMatrix"), the second one ("cacheSolve") does it. Otherwise ('If the inverse has been already computed'), "cacheSolve" just collects the inverse from the storing.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
