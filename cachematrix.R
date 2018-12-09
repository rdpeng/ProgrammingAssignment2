# Assignment: Caching the Inverse of a Matrix

# My goal: Write a pair of functions that cache the inverse of a matrix.

# I. The first function "makeCacheMatrix" creates a special "matrix" which is really 
# a list of functions.

# Description of functions:
# 1. set - set the value of the matrix
# 2. get - get the value of the matrix
# 3. setmatrix - set the value of the matrix
# 4. getmatrix - get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

# II.  This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
