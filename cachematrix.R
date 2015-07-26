## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x = NULL
  
  set <- function (y) {
    x <<- y
    inv_x <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inverse) inv_x <<- inverse
  get_inv <- function() inv_x
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$get_inv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  inv = inverse(data, ...)
  x$set_mean(inv)
  inv
}
