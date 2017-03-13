## These functions can be used to calculate and cache inverse matrices. 


## This first function creates a special matrix object that can cache inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) 
    inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function checks if the inverse matrix has already been calculated, and calculates if not.

cacheSolve <- function(x, ...) {
  inv <- x[getinv()]
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
