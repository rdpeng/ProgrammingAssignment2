## Programming Assignment 2 of R programming
##Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix computes the inverse into a cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve retrieves that inverse 

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}