## Shivam Kumar
## 10 July 2020
## This is a R program code for finding inverse of matrix.
## This program uses caching method of operation.

## first we have to make cache of a matrix.

makeCacheMatrix <- function(x = matrix()) {

  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}


## Getting inverse of the cached matrix by using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

