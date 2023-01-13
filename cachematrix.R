## This function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # inital value of matrix set to NULL
  m <- NULL
  # store matrix
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get stored matrix value
  get_matrix <- function() {
    x
  }
  # store inverse
  set_inverse <- function(inverse) {
    m <<- inverse
  }
  # get stored inverse value
  get_inverse <- function() {
    m
  }
  # return list above functions
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## cachSolve funtion omputes the inverse of the special 
## "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  # Get the cached value
  m <- x$get_inverse()
  # If cached value exist, return it
  if (!is.null(m)) {
    message("getting cache data")
    # return cached value
    m
  }
  # else get matrix and calculate inverse and store it
  matrix_value <- x$get_matrix()
  m <- solve(matrix_value, ...)
  x$set_inverse(m)
  # return inverse m
  m
}

