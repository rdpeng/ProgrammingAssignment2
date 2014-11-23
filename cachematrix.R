## Matrix Inversion is a costly computation. So we benefit by caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions below caches the inverse of a matrix. Here we assume that the input is always a square matrix.

## The function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(matrix) m <<- matrix
  getInvMatrix <- function() m
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)	
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...){
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInvMatrix(m)
  m
}

