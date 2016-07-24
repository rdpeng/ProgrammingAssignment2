## In this assignment we have to create a matrix that can be cached using the
## 'makeCacheMatrix' function. After that we have to calculate the inverse of the
## matrix that we made and have R store it in cache. In order to compute the inverse
## with the 'Solve' function (in this assignment 'cacheSolve') the matrix has to be
## square (equal number of rows and columns).

## The 'makeCacheMatrix' function creates a special matrix that can be 'cached'
## so that R doesn't have to compute whatever you want it to compute (in
## this assignment: the inverse of the matrix) everytime you use the function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## The 'Solve' function calculates the inverse of the argument you use in the
## function. In this assignment we will use this function on a matrix, so it
## will return the inverse of the matrix we made in R. The 'cacheSolve' function
## will work in the same way, except that now it will look for the answer in the
## cache if it has been computed before, so that it doesn't have to compute it
## every time. This is especially useful when you are working with large matrices.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
