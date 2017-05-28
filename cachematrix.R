##The first function defines the matrix that is to be inversed and the second funciton takes its inverse,
##if the first funciton is already inversed it retrieves it from the cache.


## Define the matrix and store it in makeCacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) i <<- solveMatrix
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the invesre of the matrix defined by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
