## Functions related to Programming Assignment 2: Lexical Scoping (R Programming, Data Science, Johns Hopkins University)


## Function to create an inversible matrix object capable to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to compute the inverse of the object created in makeCacheMatrix function if not already cached.
## If the inverse was already computed and cached, returns the cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Inverse already computed... Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}