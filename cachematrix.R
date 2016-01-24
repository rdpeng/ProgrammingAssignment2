## Put comments here that give an overall description of what your
## functions do


## Takes the matrix and stored it.
## If the Inverse matrix was ever calculated before, then that value is also cached in
## variable 'InverseMatrix'
## The original matrix can be obtained by calling 'getMatrix'.
## Inverse by calling 'getInverseMatrix'
## Set new matrix by calling 'setMatrix' with matrix as parameter.
## Set the inverse matrix by calling 'setInverseMatrix' with inverse matrix as parameter

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <-NULL
  
  setMatrix<-function(y){
    x<<-y
    InverseMatrix<<-NULL
  }
  getMatrix <-function() x
  setInverseMatrix <-function(invM) {InverseMatrix<<-invM}
  getInverseMatrix <-function(){ InverseMatrix}

    list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInverseMatrix= setInverseMatrix,  getInverseMatrix=getInverseMatrix)
}

## Takes the function that have the matrix in a variable and 
## calculates the inverse of the matrix if there is no inverse already 
## cached. If already cached, then returns it from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InverseMatrix<-x$getInverseMatrix()
  
  if(!is.null(InverseMatrix)){
    message("Inverse Matrix from Cache")
    return(InverseMatrix)
  }
  mtrx<-x$getMatrix()
  InverseMatrix<-solve(mtrx)
  
    x$setInverseMatrix(InverseMatrix,...)
  
  InverseMatrix
}
