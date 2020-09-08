## Put comments here that give an overall description of what your
## functions do
After assuming that the matrix supplied is invertible,
the 'makeCacheMatrix'function creates a special “matrix” object that can cache its inverse.
the'cacheSolve function' computes the inverse of the special “matrix” returned by 'makeCacheMatrix'. 
If the inverse has already been calculated, then 'cacheSolve' has to retrieve the inverse from the cache.
so these pair of functions below cache the inverse of a matrix.

## Write a short comment describing this function
the 'makeCacheMatrix'function creates a special “matrix” object that can cache its inverse.
The first function is a list containing a function to
1.set the matrix
2.get the matrix
3.set the inverse
4.get the inverse

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


## Write a short comment describing this function
the'cacheSolve function' computes the inverse of the special “matrix” returned by 'makeCacheMatrix'.

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