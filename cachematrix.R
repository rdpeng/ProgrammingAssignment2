## The functions below are used to cache the inverse of a matrix
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x<<- y
    m<<- NULL
  }
  
  get<- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m 
  list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of a matric.
## If the inverse has not already been computed, it computes the inverse and sets the value in the cache using the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}

mymatrix<- matrix(c(2, 4, 3, 5, 3, 1, 6, 2, 8), nrow = 3, ncol = 3, byrow =TRUE)
m1<-makeCacheMatrix(mymatrix)
cacheSolve(m1)
