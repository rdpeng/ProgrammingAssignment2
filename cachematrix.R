## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function builds an object with 2 attributes, a matrix and it's inverse, and 4 methods for accesing and modifying those attributes.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## This function receives an special object (makeCacheMatrix) and modifies its inverse and returns it.
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
