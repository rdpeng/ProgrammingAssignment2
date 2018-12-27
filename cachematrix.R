## Caching the Inverse of a Matrix.

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setM <- function(inverse) m <<- inverse
  getM <- function() m
  list(set = set,
       get = get,
       setM = setM,
       getM = getM)
}


##  This function computes the inverse of the special “matrix” returned by make 
##  CacheMatrix above. If the inverse has already been calculated (and the matrix 
##  has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getM()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setM(m)
  m
}
