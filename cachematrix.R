## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
#* set  matrix
#* get matrix
#* set_inv inverse matrix
#* get_INV inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv_m <<- solve
  get_inv <- function() inv_m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv_m <- x$get_inv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$set_inv(inv_m)
  ## Return a matrix that is the inverse of 'x'
  inv_m
}
