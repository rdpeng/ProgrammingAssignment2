## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 
## The following pair of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverseMat <- function(inverseMat) m <<- inverseMat
  getinverseMat <- function() m
  list(set = set, get = get, 
       setinverseMat = setinverseMat, 
       getinverseMat = getinverseMat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverseMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrixData <- x$get()
  m <- solve(matrixData, ...)
  x$setinverseMat(m)
  m
}