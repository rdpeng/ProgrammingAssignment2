## the makeCacheMatrix creates a list of functions that will be used to call from to either the stored inverse of a function or solve for the inverse and store it

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inverse <<- inverse
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the cacheSolve function allows you to call from the makeCacheMatrix object to accutely call the stored inverse or calculate the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() 
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverse <- inverse
  inverse
}