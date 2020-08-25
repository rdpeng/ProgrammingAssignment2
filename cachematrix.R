## CreatingthevectoranddoingMatrixfunctions
## can cache its own object.
## Function makeCacheMatrix gets a matrix as an input, set the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
         invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x 
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix 
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)

}

## function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
#input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.

cacheSolve <- function(x, ...) {
       cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) { 
    message("Getting Cached Invertible Matrix")
    return(invMatrix)
  }
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)
  x$setInverse(invMatrix)    
  return(invMatrix) 
}

 
