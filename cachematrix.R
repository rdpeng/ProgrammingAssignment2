## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that caches a square invertible matrix "x"
## which will return a list containing functions to: set the matrix, get the matrix, set the inverse, 
## and get the inverse. The list of functions will serve as the input for cacheSolve().

makeCacheMatrix<- function(x = matrix()) {
  inv = NULL
  set = function(y) {
      x<<- y 
      inv<<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function will take the list of functions from makeCacheMatrix () and produce the inverse of the original 
## matrix in makeCacheMatrix (). If the inverse has already been calculated, then it will retrieve it from the cache
## and skip the calculation. Otherwise it will calculate the inverse. Then the function sets the value of the inverse
## in the cache using the setinv function.

cacheSolve<- function(x, ...) {
  inv = x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
