## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
## Creates a special "matrix" object that can cache its inverse  
  inverse_of_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_of_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_of_x <<-inverse
  getinverse <- function() inverse_of_x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse_of_x <- x$getinverse()
  if (!is.null(inverse_of_x)) {
    message("caching inversed matrix")
    return(inverse_of_x)
  } 
  else {
    inverse_of_x <- solve(x$get())
    x$setinverse(inverse_of_x)
    return(inverse_of_x)
  }
}
