## makeCacheMatrix creates a object handler for the function makeCacheMatrix which further encapsulates
## the set,get,set_inv & get_inv functions for the matrix data supplied as argument to this function.cacheSolve function
computes the inverse and stores in cache.

## the function below creates the special matrix object in relation to matrix supplied in its argument

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get<-function() x
  set_inv<-function(inverse) inv<<-inverse
  get_inv<-function() inv
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## Returns a matrix that is the inverse of 'x' first by checking from cache and if unavailable
## computing it

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data)
  x$set_inv(inv)
  inv
}
