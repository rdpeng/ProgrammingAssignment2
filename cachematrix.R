## A set of functions to make an object that stores matrix 
## and can cache its inverse for later retrieval. 

## Function to make an object to store a matrix and cache its inverse
## Parameter:
##  x : matrix to be stored
## Returns a list of the following functions
##  set(y) : set the cached matrix to y, this will invalidate the cached 
##           inverse matrix
##  get()  : get the cached matrix
##  setinverse(i) : set the inverse of the matrix to i
##  getinverse()  : get the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # Environment variables:
  #   x       : the matrix being cached
  # inverse X : inverse of X
  
  inverseX <- NULL
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  get <- function() x
  setinverse <- function(invX) inverseX <<- invX 
  getinverse <- function() inverseX
  list(set = set, get = get, 
    setinverse = setinverse,
    getinverse = getinverse)
}


## Calculate the inverse matrix of a given cached matrix object, x.
## x should be created using makeCacheMatrix
## Parameters:
##   x : cached matrix object, created using makeCacheMatrix
##  ...: arguments to be passed to function solve
## Side effects: 
##   the cached matrix inverse stored in x will be updated
## Return value:
##   The inverse of matrix stored in object x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


