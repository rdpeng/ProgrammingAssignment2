## the objective is to write 2 functions that cache the inverse of a matrix
## by the names of "makeCacheMatrix" and "cacheSolve" 

## makeCacheMatrix function creates a special matrix object that can cache the
## inverse of input, an invertible square matrix

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


## cacheSolve function calculates the inverse of the resulting matrix by 
## makeCacheMatrix. If the inverse is already calculated and there are no changes
## in the matrix then the cachesolve will return inverse from cache

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