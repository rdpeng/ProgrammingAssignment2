## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(t) {
      m <<- t
      inv <<- NULL
    }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix created in the makeCacheMatrix function
## only if one does not exist in cache

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of the matrix 'm'
  inv <- m$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setinverse(inv)
  inv
}

