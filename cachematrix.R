## Input x: a square invertible amtrix
## return: a list containing functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse 
## 4. get the inverse 

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Input: output of makeCacheMatrix()
## return: inverse of the original matri input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}
