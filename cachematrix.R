## The following two functions work hand in hand to fasten inversion of a matrix by storing a first time
## computation in cache memory, so the inverse remains accessible for later use without having to inverse
## the same matrix any more than once.


## makeCacheMatrix allocates some memory space to construct a special matrix which stores and cache
## information on both the initial matrix 'x' and its available inverse 'inv', which by default is set to NULL
## meaning no computation has yet been made. makeCachematrix returns a list of methods and their current values,
## where the setters are defined by lexical scoping so their redefinition affect the parent frame

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
   x <<- y
   inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  ## make available the list of the previous getters and setters
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## When called a first time cacheSolve simply computes the inverse of 'x' with the solve() function.
## If called again, cacheSolve detects it has been called already and simply return the value of the inverse
## of 'x' it had kept in cache memomry.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
