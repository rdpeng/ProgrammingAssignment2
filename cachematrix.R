## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  list(get = get,
       set = set,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(is.null(inv)) {
    inv <- solve(x$get()) 
    x$setinv(inv)
    inv
  }
  else {
    print("cached inverse:")
    inv
  }
        ## Return a matrix that is the inverse of 'x'
}
