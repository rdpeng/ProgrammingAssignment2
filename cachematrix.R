## Put comments here that give an overall description of what your
## functions do
##This function creates a special "matrix" object that can cache its inverse.
##For this assignment, assuming that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  
  ## x square invertible matrix
  ## this function returns : a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inverse <<- inverse 
  getinverse = function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## x is the output of makeCacheMatrix()
  ## returns inverse of the original matrix input to makeCacheMatrix()
  
  inverse <- x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inverse)){
    #skip computation
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise, calculates the inverse 
  matrix.data <- x$getinverse()
  inverse <- solve(matrix.data, ...)
  
  # sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(inverse)
  
  return(inverse)
}

