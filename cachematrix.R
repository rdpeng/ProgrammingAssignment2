## A set of two functions to can cache the inverse of a matrix and retrieves the result of the inverse matrix

## makeCacheMatrix is a function that creates a matrix with the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## cacheSolve is a function that retrieves the inverse of the matrix of x, which is cached in the above function makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

##Example of use of the above functions: 

Ex <- matrix(1:4, 2, 2) ##Create a matrix
cacheSolve(makeCacheMatrix(Ex)) ##Use the cacheSolve function to get the result of the inverse matrix of "Ex" which is created by makeCacheMatrix(Ex)

