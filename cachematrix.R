## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will be requiring a matrix as an input, and the matrix's inverse will be saved in the vector.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve will first check, whether the matrix is already saved in the Cache or not. If yes, then it will take the output from there
## only, otherwise, it will solve the new matrix which isn't saved in the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinverse(inv)
  inv
}
