## Put comments here that give an overall description of what your
## functions do

## The first function creates a special matrix which is a list of functions
## as follows:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inversed matrix
## 4.get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  setMatrix <- function(y){
    x <<- y
    I <<- NULL 
  }
  getMatrix <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)

}


## The function below calculates the inversed matrix of the special
## matrix created in the first functio. It first checks if the reversed
## matrix has already been calculated. If so, it gets the reversed matrix 
## without extra computing. Otherwise, it calculates the inversed matrix of 
## and sets the value of the inversed matrix in the cache via setInverse function

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  m
}
