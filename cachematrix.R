#avoid potentially costly repeated computing of inverse of matrix

# create a list to 
# set value of matrix
# get value of matrix
# set value of inverse of matrix
# get value of inverse of matrix

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve checks to see if the inverse is in memory, then computes
#it if necessary
#it assumes the matrix is invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached")
    return(inv)
  }
   else{
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv}
}
