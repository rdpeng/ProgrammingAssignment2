##The storematrix function below contains 
##four functions that perform the following:
##1. Sets the value of a matrix
##2. Gets the value of a matrix
##3. Sets the inverse of a matrix
##4. Gets the inverse of a matrix

storematrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##The cachematrix function below contains
##one function which does the following:
##1. Returns the cached inverse of a matrix.
##Additionally it computes, stores, and displays
##the inverse of matrices not previously computed.

cachematrix <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}




##An example of these functions in use would be as follows:
##startmatrix<-matrix(1:4,2,2)
##stored<-storematrix(startmatrix)
##cachematrix(stored)