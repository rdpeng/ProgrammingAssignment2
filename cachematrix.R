## Put comments here that give an overall description of what your
## functions do
#-------------------------------------------------------------


## The makeCacheMatrix function creates a 'matrix' object  that
## can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() {inverse}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function. If the inverse has already been calculated 
##(and the matrix has not changed), then it retrieves the inverse from the cache
## and prints a message : "getting cached data".

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(s)
  inverse
}

