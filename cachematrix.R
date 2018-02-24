## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  stor_mat <<- x
  inv <<- solve(stor_mat)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  if(identical(x, stor_mat)) {
    message("getting cached data")
    return(inv)
  } else {
    inv <<- solve(x)
    return(inv)
  }
}