## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a special "matrix" that can cache its inverse for inputs
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function()inv
  list(set = set, get = get, setinv = setinv, getinv = getinv )
}

## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix"
## then the cachesolve should retrieve the inverse from the cache
## e.g. if X is a square invertible matrix, then solve(X) returns to its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
