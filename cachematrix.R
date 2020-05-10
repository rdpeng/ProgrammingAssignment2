## This script uses the operator <<- for assigning the value 
##to matrix object in athe environment that is different from 
##its current environment.
## This program creates a special object to store matrix and cache its inverse

## The makeCacheMatrix creates a special "matrix" with following four functions
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve)
    m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The cacheSolve function calculates inverse of a matrix using solve() function
##the inverse is calculated for the special matrix created above.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix using solve() function and sets the value of the solve 
##in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
