## We can cache the inverse of a matrix by forming a pair of functions

makeCacheMatrix <- function(x = matrix()) {
  inv_a <- NULL
  set <- function(y) {
    x <<- y
    inv_a <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_a <<- inverse
  getInverse <- function() inv_a
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve is a function that computes the inverse of "matrix", 
## which was created by makeCacheMatrix in the section.


cacheSolve <- function(x, ...) {
  inv_a <- x$getInverse()
  if (!is.null(inv_a)) {
    message("getting cached data")
    return(inv_a)
  }
  mat <- x$get()
  inv_a <- solve(mat, ...)
  x$setInverse(inv_a)
  inv_a
      
}
n <- matrix(rnorm(25),5,5)
n1 <- makeCacheMatrix(n)
cacheSolve(n1)
