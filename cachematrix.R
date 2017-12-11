# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here). The assignment 
# is to write a pair of functions that cache the inverse of a matrix.


# makeCacheMatrix creates a special, 'cachable' matrix that can be called instead of always
# recalculating the matrix. Similar to the example of caching the mean of a vector, makeCacheMatrix
# contains functions to

# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(m_inv) m <<- m_inv
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}



# cacheSolve checks to see if the matrix has already been stored in cache. If it is,
# cacheSolve retrieves the inverse from the cache and informs the user. Otherwise,
# it calculates the inverse normally.


cacheSolve <- function(x, ...) {
  m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
}