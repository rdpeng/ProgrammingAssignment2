## Programming assignment 2, two functions.  The first function makeCacheMatrix 
## calculates inverse of a matrix and stores it in cache to avoid unnecessary 
## recalculations.  

makeCacheMatrix <- function(x = matrix()){
  inv<- NULL
  set<- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
    setinverse<- function(inverse) inv <<- inverse
    getinverse <- function () inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix created by the function makeCacheMatrix.
##  If the inverse of this matrix has been calculated the inverse of the matrix is not recalculate
## but retrieved from  cache.

cacheSolve<- function(x, ...) {
  message("calculating inverse")
  inv = x$getinverse()
  if (!is.null(inv)) {
    message("fast results")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinverse(inv)
  return(inv)
}
