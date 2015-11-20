################################################################
## Overview
##
## 1.  makeCacheMatrix creates a special type of matrix 
## (object with methods and properties) that will save 
## (cache) a copy of its inverse after the first call to 
## cacheSolve
##
## 2. cacheSolve takes an argument of a special matrix,
## created by makeCacheMatrix and returns the inverse either 
## by checking to see if it's already created and if not
## goes ahead and calculates the inverse (no checking for 
## invertibility) and lets the "special matrix" store (cache)
## the value.
################################################################

################################################################
## makeCacheMatrix - create and return a "special matrix"
##
## parameter: x a matrix
##
## returns:   a special matrix (a list of functions that can
##            reference the associated cached value of the 
##            inverse of x if it exists)
##
## usage ex:  x <- matrix(1:4, 2, 2)
##            y <- makeCacheMatrix(x)
################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  ##
  ## Rudimentary checks:
  ##
  ## 1. check to make sure a parameter was provided
  ## 2. check to make sure the parameter is a matrix
  ## 3. check to make sure the matrix is square
  if (is.na(x) || !is.matrix(x)) {
    print("1st argument needs to be a matrix")
    return
  }
  
  matrixDimension = dim(x)
  if (length(matrixDimension) != 2 || 
        matrixDimension[1] != matrixDimension[2]) {
    print("This function requires a square matrix")
    return
  }
  
  mInverse <- NULL
  set <- function(y) {
    ##
    ## Preserved values of our base matrix
    ## and its associated inverse
    x <<- y
    mInverse <<- NULL
  }
  ##
  ## method to return our base matrix
  get <- function() x
  ##
  ## method to assign the value of the inverse to our base matrix
  setInverse <- function(inverseValue) mInverse <<- inverseValue
  ##
  ## method to retrieve the stored inverse to our base matrix
  getInverse <- function() mInverse
  ##
  ## return value is the "special" matrix which has the associated
  ## functions set, get, setInverse, getInverse that are assigned
  ## to the internal variables x and mInverse
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}

################################################################
## cacheSolve - return the cached value of an inverse for a
##              "special matrix" if available, otherwise 
##              calculate the inverse and save (cache) the value
##
## parameter: x, a "special matrix" (constructed by makeCacheMatrix)
##
## returns:   The inverse of x, a "special matrix" by checking
##            to see if a cached value exists and returning it
##            or calculating the inverse, caching it, and
##            returning the inverse
################################################################
cacheSolve <- function(x) {
  ##
  ## get the cached version of the inverse for the "special matrix"
  mInverse <- x$getInverse()
  ##
  ## check to see if the cached version has been calculated yet
  if(!is.null(mInverse)) {
    ##
    ## return the cached value of the "special matrix" inverse
    message("getting cached data")
    return(mInverse)
  }
  ##
  ## retrieve the base matrix of the "special matrix" passed in
  data <- x$get()
  ##
  ## calculate the inverse of the base matrix
  mInverse <- solve(data)
  ##
  ## cache the value of the inverse of the base matrix
  x$setInverse(mInverse)
  ##
  ## return the value of the inverse of the base matrix
  mInverse
}
