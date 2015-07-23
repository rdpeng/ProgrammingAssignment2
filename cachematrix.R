## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list containing other functions to be used
makeCacheMatrix <- function(x = matrix()) {
  ##variable to store matrix being returned
  invMx <- NULL
  
  ##Set the source matrix whose inverse is to be calculated
  ##Also once a new matrix is set, we need to clear the  cache
  ##so we set the invMx to NULL
  setMatrix <- function(y = matrix()){
    x <<- y
    invMx <<- NULL
  }
  
  ##Return the source matrix
  getMatrix <- function(){
    x
  }
  
  ##Set the cached matrix to be used in subsequent runs of the same object
  setInverseMatrix <- function(invMatrix){
    invMx <<- invMatrix
  }

  ##Return the cached Inverse Matrix
  getInverseMatrix <- function(){
    invMx
  }
  
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getInverseMatrix()
  if(!is.null(mx)){
    message("getting cached matrix")
    return (mx)
  }

  ##solve for inverse of matrix
  invmx <- solve(x$getMatrix(),...)
  
  ##set the cached inverse matrix   
  x$setInverseMatrix(invmx)
  
  ##Return the inverse
  invmx
}
