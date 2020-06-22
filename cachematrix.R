## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Programming Assignment Week 3 of R Programming

makeCacheMatrix <- function(x = matrix()) {            ## define the argument with default mode of "matrix"
  invMatrix <- NULL                                    ## initialize inv as NULL; will hold value of matrix inverse 
  
  #set the value of the Matrix
  setMatrix <- function(y) {                           ## define the set function to assign new 
    x <<- y                                            ## value of matrix in parent environment
    invMatrix <<- NULL                                 ## if there is a new matrix, reset inv to NULL
  }
  
  getMatrix <- function() x                              #get the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
  getInverse <- function() invMatrix                     #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object



cacheSolve <- function(x, ...) {
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               #return the invertible matrix
}