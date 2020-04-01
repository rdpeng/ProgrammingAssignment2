## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

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


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                      
    message("Getting Cached Invertible Matrix")   
    return(invMatrix)                            
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                    
  invMatrix <- solve(MatrixData, ...)            
  x$setInverse(invMatrix)                        
  return(invMatrix)                               
}
