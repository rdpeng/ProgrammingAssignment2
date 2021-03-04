## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a  
## “matrix”, a list containing a function
## to: set the value of the matrix, get the value 
## of the matrix, set the value of the inverse, and 
## then get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function evaluates the inverse of the 
## “matrix” given by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retreiving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
