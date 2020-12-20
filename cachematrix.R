#This functions create a new type of matrix capable of storing in cache it's own inverse
# once it's calculated once.

## Uses functions in function and lexical scope to carry the matrix along.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(data) {
    x <<- data
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## If not calculated previously, solve the inverse of the matrix and store it as cache. If it's already
#  in the cache, returns the result without further calculations

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    print("Cached inverse is:")
    return(inverse)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  print("Recently generated cached inverse is:")
  inverse
}

