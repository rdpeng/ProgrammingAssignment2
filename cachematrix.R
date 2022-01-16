## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## There are two functions that cache the inverse of a matrix.
## makeCacheMatrix is my first function which creates a special matrix object that can cache the inverse.

makeCacheMatrix <- function(m = matrix()) {
  inverse_matrix <- NULL
  set <- function(i){
  m <<- i
  inverse_matrix <<- NULL
}
  get <- function() m
  setInverse <- function(solveMatrix) inverse_matrix <<- solveMatrix
  getInverse <- function() inverse_matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This is my second function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- m$getInverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- m$get()
  inverse_matrix <- solve(data)
  m$setInverse(inverse_matrix)
  inverse_matrix      
}

