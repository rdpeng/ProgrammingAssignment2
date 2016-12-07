## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse mat")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
 