## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function will take a matrix as an input and store the matrix as well as its inverse if taken.
## cacheSolve function will find if the inverse of matrix is in the function makeCacheMatrix, if it is now present
#  then it will take original matrix from that previous function and set an inverse of it using solve function and
# returns the inverse matrix.








## Write a short comment describing this function

## makeCacheMatrix function will be taking a matrix as an input, set and get the values of matrix
#  accordingly. After which set and get inevrse of matrix accordingly

## Assign operator '<<-' will assign a value to an object that is in different environmnet.

makeCacheMatrix <- function(x = matrix()) {
  MatInverse <- NULL                  ## Initialize the inverse matrix
  
  setMat <- function(y){              ## set the matrix
    x <<- y
    MatInverse <<- NULL
    
  }
  getMat <- function()                ## get the matrix
    x
  setInv <- function(inverse)         ## set the inverse of matrix
    MatInverse <<- inverse
  getInv <- function()                ## get the inverse of matrix
    MatInverse
  list(setMat = setMat,getMat = getMat,
       setInv = setInv,getInv = getInv)
}


## Write a short comment describing this function

## The function 'cacheSolve' returns an inverse of matrix in function 'makeCacheMatrix'
#  if the MatInverse value in that function is empty, for which it will take original matrix 
#  from "makeCacheMatrix' and invert the matrix using solve function.

cacheSolve <- function(x, ...) {
  MatInverse <- x$getInv()
  if(!is.null(MatInverse)){                       ## If the inverse of matrix is not null
    message("Getting the cached Matrix Inverse")  #  get the cached Inversematrix
    return (MatInverse)                           #  return the inverted matrix
  }
  orgMat <-x$getMat()                             ## Retreive the original matrix
  MatInverse <- solve(orgMat,...)                 ## Take the inverse of original matrix
  x$setInv(MatInverse)                            ## set the matrix inverse
  return(MatInverse)                              ## Return the inverted matrix
        
}
