## makeCacheMatrix creates a matrix object that can cache its inverse by 
## creating a list containig 4 functions:
##  
## $set:          creates a matrix x by assigning the values of input y
##                i (the matrix of the inverse) is set to Null  
## $get:          returns the matrix x
## $setinverse:   computes the inverse of x using solve() 
## $getinverse:   returns the i if this value has already been computed, NULL otherwise



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
## cacheSolve is a function that takes a special Matrix x (created by makeCacheMatrix)
## as argument and returns its inverse. . Has the inverse of x already been 
## computed (x$getinverse() != NULL), it returns i from the cache. 
## Otherwise it computes the value of i and sets the inverse of x in the cache to i 
##(x$setinverse(i). 


cacheSolve<- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
}

