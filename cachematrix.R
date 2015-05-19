## Performant matrix operations using caching w/in a
## closure.  Currently only a solve operation provided.

## Returns a list of operations on a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Sets the underlying matrix value X
  set <- function(new_x) {
    x <<- new_x
    inverse <<- NULL
  }
  
  ## Returns the underlying matrix value X
  get <- function() x
  
  ## sets the inverse of x
  set_inverse <- function(i) inv <<- i
  
  ## gets the inverse of x
  get_inverse <- function() inv
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Solve operation on a cached matrix, returning the inverse of the matrix by
## default.  Variable arguments are passed into the solve operation when calculating
## the cached solve value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  result <- x$get_inverse()
  if (is.null(result)) {
    print('calculating inverse')
    data <- x$get()
    result <- solve(data, ...)
    x$set_inverse(result)
  }
  result
}


## Not asked for, but here is how all of this might be combined into a single
## function in a more OO style w/no side effects to operations.

## Constructor for a CachedMatrix that exposes operations on an underlying
## matrix whose results are computed once and cached.  The cached values are
## returned thereafter.
CachedMatrix <- function(x) {
  self.x <- matrix(x, nrow=nrow(x), ncol=ncol(x))
  self.inverse <- NULL
  
  ## the value of the CachedMatrix, i.e. x
  value <- function() matrix(self.x, ncol=ncol(self.x), nrow=nrow(self.x))
  
  ## the inverse of the CachedMatrix, i.e. solve(x)
  inverse <- function() {
    if (is.null(self.inverse)) {
      print('calculating inverse')
      self.inverse <<- solve(x)
    }
    self.inverse
  }
  
  list(value=value, inverse=inverse)
}