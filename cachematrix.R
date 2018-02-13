#The purpose of this file is write two functions, which helps calculating inverse of matrix using caching.

#The first function called makeCacheMatrix creates a special 'matrix', which in the fact is a list of 4 functions:
#1. set the matrix
#2. get the matrix
#3. set inverse of matrix
#4. get inverse of matrix



makeCacheMatrix <- function(X = matrix()) {
  Inv <- NULL
  set <- function(Y) {
    X <<- Y
    Inv <<- NULL
  }
  get <- function() X
  setInverse <- function(solve) Inv <<- solve
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Function 'cacheSolve' calculates inverse of matrix. The input of the function is special matrix produced by function 'makeCacheMatrix'.
#Before any calculation, function checks, if any inverse has already cached. If so, then value is taken from the cache and calculations are omitted.


cacheSolve <- function(X, ...) {
  Inv<- X$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setInverse(m)
  m
}
