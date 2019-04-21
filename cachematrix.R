## the first function will create an inverse of a mnatrix (if possible) and store it in cache for later use
## the second function will direct itself to that cache and use it's contents rather than recomputing the result
## caching the objects will allow for a one time computation and generate efficiency in the coding

## this function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
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


## this function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
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
