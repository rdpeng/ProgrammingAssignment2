## File: cachematrix.R
## Date: 2016-01-24
## Author: Alexey Kulikov <alexeyqu at gmail dot com>
## Description: 
## Small demo functions for the 2nd assignment of R course on Coursera.
## Given a square invertible matrix, they perform calculation of inversed matrix
## using standart solve() function 
## AND storing the result in cache to speed up repeated calculations.

## That is a "wrapper" function for the standart matrix type that stores cached inversed matrix

makeCacheMatrix <- function (matr = matrix()) {
  cachedInversedMatrix <- NULL
  
  setMatr <- function (newMatr) {
    matr <<- newMatr
    cachedInversedMatrix <<- NULL
  }
  
  getMatr <- function() matr
  
  setInvMatr <- function (invMatr) {
    cachedInversedMatrix <<- invMatr
  }
  
  getInvMatr <- function() cachedInversedMatrix
  
  list(setMatr = setMatr, getMatr = getMatr,
       setInvMatr = setInvMatr,
       getInvMatr = getInvMatr)
}


## The version of solve() function that uses cached data, if possible

cacheSolve <- function(matr, ...) {
  invMatr <- matr$getInvMatr()
  if (!is.null (invMatr)) {
    message ("cached data returned")
    
    return(invMatr)
  }
  
  data <- matr$getMatr()
  invMatr <- solve (data, ...)
  matr$setInvMatr (invMatr)
  
  invMatr
}
