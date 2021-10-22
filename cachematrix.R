## The function cache the computatiion of inverse a non-singular matrix.

## The function is a wrapper of setter and getter functions that calculate and
## retrieve the value of the original and inverse matrices.

makeCacheMatrix <- function(X = matrix()) {

  inverse_X <<- NULL
  
  # Setter for the original matrix
  set <- function(matrix_) {
    X <<- matrix_
    inverse_X <<- NULL
  }
  
  # Getter for the original matrix
  get <- function() X
  
  # Setter for the inverse matrix
  set_inverse <- function(inverse_matrix) inverse_X <<- inverse_matrix
  
  # Getter for the inverse matrix
  get_inverse <- function() inverse_X
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The function try to retrieve the value of the inverse matrix if existed, 
## else try to solve for the inverse matrix.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
  
  # Try to retrieve the value of the inverse matrix
  inverse_X <- X$get_inverse()
  
  # If possible then return the value and end the function's call
  if(!is.null(inverse_X)) {
    message("getting cached data")
    return(m)
  }
  
  # Else try to solve for the inverse matrix
  data <- X$get()
  inverse_X <- solve(data, ...)
  X$set_inverse(inverse_X)
  
  # Return the value of the inverse matrix
  inverse_X
}
