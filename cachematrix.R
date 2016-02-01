## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object to cache its inverse
## Returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
## Setter and getter for matrix
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }

  get <- function() x
## Setter and getter for inverse 
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  
## Put functions in list object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
## Gt inverse of function x from cache   
  inv <- x$getInverse()
## If in cache return it 
  if (!is.null(inv)) {
     return(inv)
  }
## Get matrix
  mat <- x$get()
## calc inverse of it
  inv <- solve(mat, ...)
## Set inverse  in cache
  x$setInverse(inv)
## Return 
  inv
}

a <- matrix(1:4, 2, 2)
b <- makeCacheMatrix(a)
cacheSolve(b)
