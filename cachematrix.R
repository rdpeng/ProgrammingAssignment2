## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getinverse <- function() (inv)
  list(set = set, get = get,
       setInverse = setInverse,
       getinverse = getinverse)
}

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$getinverse(inv)
  inv
}

