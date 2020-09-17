## Put comments here that give an overall description of what your
## functions do

rm(list = ls())

makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL
  
  set <- function(y) {
      x <<- y
      matrix_inverse <<- NULL
  }
  
  ## ls(environment(funs$set))
  ## [1] "get"        "getInverse" "inv"        "set"        "setInverse" "x"
  get <- function() x
  setInverse <- function() matrix_inverse <<- solve(x) #calculate the inverse
  getInverse <- function() matrix_inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  matrix <- x$getInverse()
  
  if (!is.null(matrix)) {
    message("getting cached inverse matrix")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  ##x$setInverse(matrix)
  setInverse <- function() matrix <<- solve(x)
  
  matrix
}
