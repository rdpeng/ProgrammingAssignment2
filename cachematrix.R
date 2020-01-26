## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# the matrix x is the funcation input
makeCacheMatrix <- function(x = matrix()) {

  inevertMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    inevertMatrix <<- NULL
  }
  ##seters and geters 
  getMatrix <- function()x
  setInverse <- function(inverse) inevertMatrix <<- inverse
  getInverse <- function() inevertMatrix 
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MyInverseMatrix <- x$getInverse()
  if(!is.null(MyInverseMatrix)) {
    message("getting cached data")
    return(MyInverseMatrix)
  }
  ##if invarse matrix does nnot exsist in cache
  MyMatrixToInverse <- x$getMatrix()
  setInverse <- solve(MyMatrixToInverse, ...) # invert the metrix using solve()
  x$setInverse(MyInverseMatrix) ### set the invert  metrix
  return (MyInverseMatrix)
}
