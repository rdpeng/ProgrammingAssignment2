##inverse of a matrix is cached by a pair of functions
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(n){
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setInverse(inv)
  inv      
}
