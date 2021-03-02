##This function receive a square matrix "x" and gives an special object
##like a structure includind matrix "x"

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mi <<- solve
  getinverse <- function() mi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function receive an "special object" with a square matrix.
## store and return inverse matrix of X. If the matrix has not changed, 
## inverse matrix is simply called from memory.

cacheSolve <- function(x, ...) {
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinverse(mi)
  mi
}
