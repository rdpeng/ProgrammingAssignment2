## The functions serve to
## 1. Define a special "matrix" object, which allows operating with and caching of the matrix inverse
## 2. Perform operations on the above "matrix" object

## Creates a list of functions (= "matrix" object) for a given matrix: get/set the value, get/set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(val) inverse <<- val
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Computes the inverse of the matrix encoded in the "matrix" object 
## and caches the result in the "matrix" object for subsequent quick retrieval

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
