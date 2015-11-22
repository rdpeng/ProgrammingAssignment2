## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a "special" matrix holding inside two variables:
## x the data of the matrix
## inverse is the inverse of x (could be calculated with the function cacheSolve) 
## example
## mySpecialMatrix <- makeCacheMatrix(matrix(data = c(4, 7, 2, 6), nrow = 2, ncol = 2, byrow = TRUE))

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(xInverse) inverse <<- xInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function sets the inverse of our "special" matrix and returns it
## The input parameter must be a "special" matrix created by the function makeCacheMatrix
## example
## mySpecialMatrix <- makeCacheMatrix(matrix(data = c(4, 7, 2, 6), nrow = 2, ncol = 2, byrow = TRUE))
## cacheSolve(mySpecialMatrix)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
