# This function creates a special "matrix" object that can cache its inverse.
# It checkes whether inverse of the matrix is already stored in cache. If it
# is: the function will return the inversed matrix from the cache, 
# if not: the function will calculate the the inversed matrix and store in cache.


## The function makeCacheMatrix reads in the matrix and sets its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## This function cacheSolve checkes whether the inverse of the matrix is stored in cache. 
## In the case it stored, the function reads and returns it from cache.
## In the case it is not, the function calculates the inverse and stores in cache.

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
