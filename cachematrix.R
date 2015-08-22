## This is a set of functions that creates a special matrix that contains
## the matrix and a set of functions to operate on that matrix.
## Now given that matrix, the 2nd function then calculates inverse of that
## Matrix and stores that inverse in a cache to be used again if needed

## This is the function that takes a regular matrix and converts it into
## a special matrix. This special matrix contains the actual matrix as well as
## functions that can manipulate the matrix such as set and get functions as
## well as setinverse and getinverse which either gets the already stores inv
## or sets the newly computed inverse

makeCacheMatrix <- function(x = matrix()) {
  ## creating a matrix and then its inverse is stored in cache
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function( inverseM ) inv <<- inverseM
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function solves for the inverse of a matrix. First one needs to create
## the meta matrix using the makeCacheMatrix function which creates the meta
## matrix. Then this function operates on this matrix. If the inverse already 
## computed return the computed inverse else, compute the inverse, store the
## inverse in cache and return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
