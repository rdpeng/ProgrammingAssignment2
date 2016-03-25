##
## DATE: March 25, 2016
##
## cacheMatrix.R class exposes following two method 
## 1. makeCacheMatrix: 
## 2. cacheSolve
##
## USAGE:
## > rm(list=ls())
## > source("cacheMatrix.R")
## > m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##
## > m1modi <- makeCacheMatrix(m1)
##
## > result <- cacheSolve(m1modi)
##


##
##    makeCacheMatrix provides the DS and following 4 functions
##     1.1 set
##     1.2 get
##     1.3 setmatrix   *By naming the functions setmatrix will be helpful to cover both use-cases (inv or reg) matrix (for future).
##     1.4 getmatrix   *By naming the functions getmatrix will be helpful to cover both use-cases (inv or reg) matrix (for future).
##
##     Note: Function also uses <<- operator which can be used to assign a value to an object in an
##       environment that is different from the current environment
##
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the variable m to NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## Set Matrix can be used by calling methods to assign new inv/ reg matrix.
  setmatrix <- function(matrix) m <<- matrix
  
  ## Get Matrix can be used by calling methods to fetch existing stored matrix value; NULL in-case nothing is stored as yet.
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##
## cacheSolve : provides the logical client implementation and utilizes makeCacheMatrix functions for caching needs.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ##
  m <- x$getmatrix()
  
  ## First attempt is.null(m) will be NULL (TRUE); Using (!) will help us skip the condition in-case 
  ## cached returned empty.
  if(!is.null(m)) {
    ## Following code will be executed in-case m is not NULL (previously calculated)
    message("getting cached matrix data")
    return(m)
  }
  data <- x$get()
  ## Following code uses existing R function to inverse the matrix.
  m <- solve(data, ...)
  ## Following code assigns the inverse matrix to makeCacheMatrix DS for future use.
  x$setmatrix(m)
  ## following will return the inverse matrix back.
  m
}
