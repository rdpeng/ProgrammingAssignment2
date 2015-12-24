## Put comments here that give an overall description of what your
## functions do
## Pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
  getInverseMatrix <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  data <- x$getMatrix()
  inverseMatrix <- solve(data)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
