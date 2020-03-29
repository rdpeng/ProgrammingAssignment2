## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m_inv <<- inverse
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinv()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- inv(data, ...)
  x$setinv (v)
  v
}
