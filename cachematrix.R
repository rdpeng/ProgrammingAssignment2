## Coursera Programming with R: Week 3 Assignment 2

## This function creates a special "matrix" object that can cache its inverse

## Matrix inversion is usually very computationally intensive - especially for large
## size matrices.  Sometimes in code (and especially in loops), the inverse of a matrix need only be computed once.  
## To avoid recomputing the inverse and generating the same result repeatedly, we can simply compute the result once. 

## Usage example:
  ## x <- matrix(1:4, nrow=2, ncol=2)
  ## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  ## if the getmatrix function is activated, return the original input variable x as the matrix
  getmatrix <- function() x
  ## if the set matrix function is activated take the matrix to analysed and set it as x, then set the inverse to Null
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinverse <- function() inverse
  setinverse <- function(inversecache) inverse <<- inversecache
  
  ## return list of functions for matrix
  list(getmatrix=getmatrix, setmatrix=setmatrix, getinverse=getinverse, setinverse=setinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    browser()
    inverse <- x$getinverse()
    
    # return matrix inverse from cache if it exists
    if (!is.null(inverse)) {
      message("Inverse has previously been cached, retrieving now")
      return(inverse)
    }else{ ##else if it doesn't exist, we must calculate the inverse of the matrix x
    matrix <- x$getmatrix()
    browser()
    inverse <- solve(matrix, ...)
    }
    
    # once calculated, cache inverse for future
    x$setinverse(inverse)
    
    # return inverse of matrix
    return(inverse)
  }
