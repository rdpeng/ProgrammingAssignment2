## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL 
  set <- function(y) {
    x <<- y 
    invr <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invr <- x$getInverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  mat <- x$get()
  invr <- solve(mat, ...)
  x$setInverse(invr)
  invr
  
}
