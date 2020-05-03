## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a matrix object that can cache its inverse
  m <- NULL
  set <- function(y) {                     
    x <<- y                             
    m <<- NULL                      
  }
  get <- function() x                   
  
  setinverse <- function(inverse) m <<- inverse  
  getinverse <- function() m                  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## This function computes the output of the previous matrix makeCacheMatrix above
## If the inverse has already been calculated and the makeCacheMatrix has not changed,
## then the function cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

