
## Function to create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solveMatrix) inv <<- solveMatrix
  get_inverse <- function() inv
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}
## Function to compute the inverse of the special matrix returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv      
}
