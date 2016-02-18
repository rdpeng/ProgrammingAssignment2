## A pair of functions (makeCacheMatrix & cacheSolve) that cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function (y){
    x <<- y
    i <<- NULL
  }

  get <- function () x
  
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
    
  
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
  
  print(x, i)
  
}


## cacheSolve computes the inverse of the special "matrix" created in makeCacheMatrix. If the inverse has already been calculated, it retrives it from the cache.

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)

  i  
}
