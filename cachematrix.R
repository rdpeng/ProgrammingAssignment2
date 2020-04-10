## My functions are to cache the inverse of a matrix using a pair of functions 
## "makeCacheMatrix" and "cacheSolve"

## "makeCacheMatrix" is a function which creates a matrix 
## that cache its inverse for the object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, 
       setinv = setInv, getinv = getInv)
}



## "cacheSolve" computes the inverse of the matrix returned by makeCacheMatrix 
## If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  result <- x$get()
  inv <- solve(result, ...)
  x$setInv(inv)
  inv
}



