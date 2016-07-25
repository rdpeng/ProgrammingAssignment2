## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creat matrix cache the inverse of this matrix .


makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculaate the inverse of the matrix  created by makeCacheMatrix,
##if the inverse already existe of the same matrix then the function return inverse from the cache.


cacheSolve <- function(x, ...) {
  
  inve <- x$getinverse()
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  matr <- x$get()
  inve <- solve(matr, ...)
  x$setinverse(inve)
  inve
}