#matrixcache.R ---> by Frank-Hu

## Put comments here that give an overall description of what your
## functions do

## These functions were for Coursera: R Programming 
## Week 3 Assignment on 2019/09/27th by HuZiwen 
## I create this function to return a matrix whose value was cached after the process

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 invertMatrix <- NULL

#set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invertMatrix <<- NULL
  }

  getMatrix <- function() x                                 # get value of the original Matrix
  setInverse <- function(inverse) invertMatrix <<- inverse  # set value of the invertible matrix
  getInverse <- function() invertMatrix                     # get value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Function cacheSolve takes input the result of the function makeCacheMatrix(matrix)
## and checks its inverse matrix, which contains value or not.

cacheSolve <- function(x, ...) {
        ## Return a inverse matrix of  'x'
        
#get the value of the invertible matrix from the makeCacheMatrix function
          invertMatrix <- x$getInverse()
        if(!is.null(invertMatrix)) {                       # if input is not NULL
          message("getting invertible matrix cached")      
          return(invertMatrix)                             # return the matrix
        }
## If the input is empity, it will use function solve to output a inverted matrix from original matrix
        
        MatrixData <- x$getMatrix()                        # get the original Matrix Data and skip function makeCacheMatrix
        invertMatrix <- solve(MatrixData, ...)             # inverse the matrix
        x$setInverse(invertMatrix)                         # set the new matrix 
        return(invertMatrix)                               # return the new matrix
  
## If the input has value in it, it will return "getting invertible matrix cached" &  a inversed matrix
}

