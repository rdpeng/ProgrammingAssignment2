## Matrix inversion is a costly operation. If the matrix is large and
## its inverse is needed more than once, it might make sense to store
## the inverse and retrive it from a cache rather than calculate it again.

## This function takes a matrix as input and returns a list which
## can store both the matrix and the inverse if needed.

makeCacheMatrix <- function(x = matrix()) {
  ## We assume that the input is valid, i.e. an invertible matrix.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function accepts the special CacheMatrix list as input
## and returns the inverse of the matrix.
## If the inverse already has been calculated it is returned.
## Otherwise it is calculated and stored for later use in the list.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## We need to calculate the inverse.
  org_x <- x$get()
  ## We use solve to find the "right" inverse inv, such that
  ## X multiplied by inv gives the Unit Matrix.
  inv <- solve(org_x, diag(rep(1,ncol(org_x))))
  x$setinv(inv)
  inv
}
