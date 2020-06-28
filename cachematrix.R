## The first function sets and gets a matrix, and sets and gets the inverse of
## this matrix.
## The second function is used to invert the matrix and store a cached version
## of it.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(solve) inv <<- solve
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheInvMatrix <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInv(inv)
      inv
}