## Functions that cache the inverse of a matrix
## Usage example:
##
## > source('cachematrix.R')
## > outputMatix <- makeCacheMatrix(matrix(c(2, 3, 1, 4, 3, 2,1,2,3),c(3,3)))
## > cacheSolve(outputMatix)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(inputMatrix = matrix()) 
{
  mInverse <- NULL
  set <- function(y) 
    {
      inputMatrix <<- y
      mInverse <<- NULL
    }
  get <- function() inputMatrix
  setinverse <- function(inv) 
    {
      mInverse <<- inv
    }
  getinverse <- function() mInverse
  list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
      )
}

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(inputMatrix, ...) 
  {
    mInverse <- inputMatrix$getinverse()
    if(!is.null(mInverse)) 
      {
        message("Getting Cached Data")
        return(mInverse)
      }
    mat <- inputMatrix$get()
    mInverse <- solve(mat, ...)
    inputMatrix$setinverse(mInverse)
    mInverse
  }