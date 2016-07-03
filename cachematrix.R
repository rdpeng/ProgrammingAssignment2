rm(list=ls())

#First function to:
#Set Value of Vector
#Get Value of Vecotr
#Set Value of Mean
#Get value of Mean

makeCacheMatrix <- function(x = matrix()) {
  Inv = NULL
  Set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) Inv <<- inverse 
  getinv = function() Inv
  list(Set=Set, get=get, setinv=setinv, getinv=getinv)
}

#Compute the inverse of the matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
}

