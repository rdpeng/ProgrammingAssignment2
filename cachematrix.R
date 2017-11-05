## This script consists of two functions: makeCacheMatrix(x) and cacheSolve(x, ...).
## This functions combined provide a framework of getting the inverse of a matrix
## efficiently. The efficiency is gained by pulling previously computed inverses
## from cached results rather than computing them again each time.


## makeCacheMatrix(x)
# Once makeCacheMatrix(x) is run for example as myCacheMatrix <- makeCacheMatrix(x), an
# object with the name of myCacheMatrix and of type makeCacheMatrix() is instantiated and
# the environment of myCacheMatrix contains x, xInverse, setX(y), getX(), setInverse(i),
# and getInverse().
# The default value matrx() for the parameter x is needed for avoiding error messages
# in later calling myCacheMatrix$get().
makeCacheMatrix <- function(x = matrix()) {
  
  # clearing previous values of xInverse by setting it to NULL
  xInverse <- NULL
  
  # set(y) assigns y to x and NULL to xInverse. Since the assignments point to the
  # parent env. of set(y) with the assignment being (<<-), x and xInverse
  # aren't defined whithin the function set(y) itself, but get assigned a value
  # in the parent env. of set(y), which is the function makeCacheMatrix(x).
  setX <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  
  # getX() returns x, which is not defined within the function getX() itself.
  # So, x is retrieved from the parent env. of getX(), which is the function makeCacheMatrix(x).
  getX <- function() x
  
  # setInverse(i) assigns i to xInverse. Since the assignment points to the
  # parent env. of setInverse(i) with the assignment being (<<-), xInverse
  # isn't defined whithin the function setInverse(i) itself, but gets assigned a value
  # in the parent env. of setInverse(i), which is the function makeCacheMatrix(x).
  setInverse <- function(i) xInverse <<- i
  
  # getInverse() returns xInverse, which is not defined within the function getInverse() itself.
  # So, xInverse is retrieved from the parent env. of getInverse(), which is the function
  # makeCacheMatrix(x).
  getInverse <- function() xInverse
  
  # last statement as return value of the function that gets passed to the parent env.:
  # a list containing four named elements representing the subfunctions above.
  # Naming the elements of the list allows for accessing them by name with the $ operator,
  # i.e. myCacheMatrix$getX() or myCacheMatrix$setInverse(i1).
  list(setX = setX, getX = getX, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve(myCacheMatrix, ...)
# cacheSolve(myCacheMatrix, ...) expects an object myCacheMatrix of type makeCacheMatrix()
# as a parameter. Further, with '...' it allows for additional parameters that are used
# for the actual computation of the matrix inverse.
# I changed the name of the parameter from 'x' to 'myCacheMatrix' to cleary separate it
# from the parameter 'x' of the function makeCacheMatrix.
cacheSolve <- function(myCacheMatrix, ...) {
  
  # Assign the return value of the function myCacheMatrix$getInverse() to xInverse.
  # The return value of myCacheMatrix$getInverse() is NULL unless it has been set
  # by the function myCacheMatrix$setInverse() before.
  xInverse <- myCacheMatrix$getInverse()
  
  # if xInverse is not NULL
  if(!is.null(xInverse)) {
    
    # announce that a cached value can be used
    message("getting cached data")
    
    # exit the function cacheSolve(myCacheMatrix, ...) with the return value xInverse
    return(xInverse)
  }
  
  # The subsequent code is only executed when xInverse is NULL.
  # Retrieve the matrix x within myCacheMatrix with the function myCacheMatrix$get()
  # and assign it to data.
  data <- myCacheMatrix$getX()
  
  # actually compute the inverse of x
  xInverse <- solve(data, ...)
  
  # assign the computated inverse of x to xInverse within myCacheMatrix
  myCacheMatrix$setInverse(xInverse)
  
  # return xInverse
  xInverse
}

