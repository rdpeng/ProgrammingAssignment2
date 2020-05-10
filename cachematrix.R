## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}
