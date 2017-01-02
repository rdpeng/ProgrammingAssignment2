## Put comments here that give an overall description of what your
## functions do

## Creats a matrix with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ##steps for this function:
  ##set matrix
  ##get matrix
  ##set inverse
  ##get inverse
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solveMatrix) inverse <<- solveMatrix
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## Get the output of makeCacheMatrix and return its inverse

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <<- x$getinverse()
  
  ##check for previous calculation
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  tempdata <- x$get()
  inverse <- solve(tempdata)
  x$setinverse(inverse)
  inverse
}

