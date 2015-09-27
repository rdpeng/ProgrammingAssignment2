##Dan Spindler
## Assignment 2
## Sept. 26 2015


## Put comments here that give an overall description of what your
## functions do
# Function make set the value of the matrix
#  Then gets the value of the matrix
#   Next Set the value of the inverse
#     Then.  the value of the inverse


## Write a short comment describing this function
# makeCacheMatrix: return a list of functions to:
makeCacheMatrix <- function(x = matrix()) {
  # inverse_value contains inverse matrix
  inverse_value <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inverse_value <<- NULL
  }

  get <- function() x
  setinv <- function(inverse) inverse_value <<- inverse
  getinv <- function() inverse_value
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix. or returns the cached inverse.
cacheSolve <- function(x, ...) {
  inverse_value <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inverse_value)) {
    message("Get the cached data")
    return(inverse_value)
  }
 
  # calculate inverse
  data <- x$get()
  inverse_value <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inverse_value)
  
  # Return
  inverse_value
}
  
  