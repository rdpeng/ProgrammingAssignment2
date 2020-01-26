## Autore: sagyg


##desc:
## makeCacheMatrix- set of getters and Setters to store the invert matrix to the environment using <<- 
#<<- operator is used to assign a value to an object in an environment that is different from the current environment 
## cacheSolve - get the invert matrix form the cache and return it, if not exist, get the original matrix and invert it
  

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




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MyInverseMatrix <- x$getInverse()
  if(!is.null(MyInverseMatrix)) {
    message("getting cached data")
    return(MyInverseMatrix)
  }
  ##if invarse matrix does nnot exsist in cache
  MyMatrixToInverse <- x$getMatrix()
  MyInverseMatrix <- solve(MyMatrixToInverse, ...) # invert the metrix using solve()
  
  x$setInverse(MyInverseMatrix) ### set the invert  metrix
  return (MyInverseMatrix)
}
