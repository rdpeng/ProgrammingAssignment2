## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# x is the input matrix
# m is the cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function firstly determines whether matrix x has been inversed; if not, then get the 
# inverse matrix as m. Finally, return the inversed matrix m.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # determine whether the matrix x has been inversed
  if(is.null(m)) {
    message("getting the inversed matrix")
    return(m)
  }
  # otherwise, calcuate the inversed matrix as m
  M1 <- x$get()
  m <- solve(M1, ...)
  x$setmatrix(m)
  m
}

