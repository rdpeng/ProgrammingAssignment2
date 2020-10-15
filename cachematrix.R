## The functions below caches the inverse of a square matrix, first by making a 
## special "matrix" object that can cache its inverse, and then by computing the 
## inverse of the special "matrix" that is returned

## The first function 'makeCacheMatrix' creates the special 'matrix,' which is 
## really a list containing a function to: 1. set the value of a matrix, 2. get 
## the value of a matrix, 3. set the inverse value of a matrix, 4. get the
## inverse value of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' is a function which finds the inverse of 'makeCacheMatrix.'
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it finds the inverse of the matrix and sets it in the cache via 
## the setInverse function.

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
