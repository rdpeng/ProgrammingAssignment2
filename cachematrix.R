## Put comments here that give an overall description of what your
## functions do

## First part of the function will make cache matrix which will get and set the value
## of the matrix and get and set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the value of the matrix
  
  get <- function() x
  # set the value of the inverse
  
  setinverse <- function(inverse) i <<- inverse
  # get the value of the inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the inverse of the matrix. It checks if
## the inverse has already been computed. If yes, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## Test


#B <- matrix(c(1,3,5,7),2,2)


#B1 <- makeCacheMatrix(B)

#cacheSolve(B1) 
