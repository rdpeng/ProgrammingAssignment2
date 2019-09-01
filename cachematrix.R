## This pair of function will help us cache the inverse of a matrix
## in an efficient manner.

## makeCacheMatrix creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## this will be an input in cacheSolve()
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special 'matrix' returned 
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of original matrix'x'
  inv <- x$getinverse()
  ## if the inverse has laready been calculated
  if (!is.null(inv)) {
    ## gets it from cache and doesn't do the computation
    message("getting cached data")
    return(inv)
  }
  ## otherwise, computes the inverse
  mtr <- x$get()
  inv <- solve(mtr, ...)
  ## using setinverse function to set the value of the inverse
  x$setinverse(inv)
  return(inv)
}
