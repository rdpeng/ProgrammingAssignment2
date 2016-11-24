## These two functions create a special object that store a matrix and cache 
## its inverse, which can save us a lot of work.

## This function creates a special matrix which cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  get <- function() x
  set_Inverse <- function(inverse) inv <<- inverse
  get_Inverse <- function() inv
  list(set = set,
       get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


## This function computes the inverse of the previous special matrix.

cacheSolve <- function(x, ...) {
  inv <- x$get_Inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set_Inverse(inv)
  inv
}



