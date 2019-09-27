## A pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
     x <<- y
     inv <<- NULL 
  }
     get <- function() x  ## define the get fucntion - returns value of the matrix argument
     setInverse <- function(solveMatrix) inv <<- solveMatrix
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



  }
