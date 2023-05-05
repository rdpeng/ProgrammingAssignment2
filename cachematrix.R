## Put comments here that give an overall description of what your
## functions do

## First part to create matrix cache objects

makeCacheMatrix <- function(x = matrix()) {
  ## Make a function that erects the cache matrices
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Second part to actually use cached matrices, upon calling an inverted matrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', thereby using the cached matrices checks
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
}