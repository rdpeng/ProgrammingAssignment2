## These functions take a vector, and return an inverted matrix of
## the vector's values.

## This function inverts the value of the vector and caches said value.

makeCacheMatrix <- function(x = matrix()) {
  matrix_to_invert <- NULL
  set <- function(y) {
    x <<- y
    matrix_to_invert <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) matrix_to_invert <<- solve
  getmatrixinv <- function() matrix_to_invert
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}

## This function returns the value of solved cached matrices, or
## solves the value if that value was not previously cached.

cacheSolve <- function(x, ...) {
  matrix_to_invert <- x$getmatrixinv()
  if(!is.null(matrix_to_invert)) {
    message("getting cached data")
    return(matrix_to_invert)
  }
  data <- x$get()
  matrix_to_invert <- matrix(data, nrow = sqrt(length(data)))
  x$setmatrixinv(matrix_to_invert) 
  solve (matrix_to_invert)
}
