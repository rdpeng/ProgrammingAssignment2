## These two functions are for computing and caching the matrix
## inverse of an input matrix. The matrix and its cached inverse are
## stored in the closure environment returned by makeCacheMatrix, with
## relevant functions to get and set the matrix and inverse. The 
## function cacheSolve will retrieve the cached inverse, or compute it
## if a cache does not exist.



#' Constructor for instantiating the functions and closure environment
#' containing the input matrix and the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  set <- function(y) {
    if (class(y) != "matrix") 
      stop("Please give me a matrix!")
    x <<- y; 
    xInverse <<- NULL
  }
  
  setInverse <- function(...) xInverse <<- solve(x, ...)
  
  get <- function() x
  getInverse <- function() xInverse
  
  return(list(set = set, 
              get = get, 
              setInverse = setInverse, 
              getInverse = getInverse))
}



#' Get the matrix inverse: retrieve cached inverse if it has been 
#' already been cached, otherwise compute inverse.
cacheSolve <- function(x, ...) {
  xInverse <- x$getInverse()
  
  if (is.null(xInverse)) {
    x$setInverse(...)
    xInverse <- x$getInverse()
  } else {
    message("Retrieving cached inverse")
  }
  
  return (xInverse)
}
