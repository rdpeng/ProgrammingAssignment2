## Put comments here that give an overall description of what your
## functions do

## makeChacheMatrix creates and calculates the inverse of the matrix
## It will do the following operations
## SET the value of the matrix
## GET the value of the matrix
## SET the value of the Inverse of the matrix
## GET the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list ( set=set, get=get,
         setinverse=setinverse, getinverse=getinverse)
}


## The cachesolve function below returns the inverse of the matrix. 
## first it checks whether the inverse is already been computed, 
## if YES, then it displays the results which is in the Cache and 
## skips the computation. If NO, then it computes and return the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting result form chached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
