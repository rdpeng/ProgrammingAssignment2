## First, creating a special "matrix" object that can cache its inverse. Second, computing the inverse of the special "matrix"
  
makeCacheMatrix <- function (x = matrix()) {
  ## 1st a special "matrix" object that can cache its inverse
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## 2nd computing the inverse of the special "matrix"
  inv <- x$getInverse()
  if (!is.null(inv)){
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}