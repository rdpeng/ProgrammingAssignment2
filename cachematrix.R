##creating the cache where the inverse matrix can be stored

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## function checks if inverse matrix is calculated before, if this is the case it will not recompute. Else it gets the matrix and uses solve to compute the inverse.
## this value is given to setInverse (stored in the cache)

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
