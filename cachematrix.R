## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. Beneath you will find 
## a pair of functions that cache the inverse of a matrix.


## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) {
    m <<- solve
  }
  
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Calling the function
A <- matrix( 
  c(1, 2, 3, 4), # the data elements 
  nrow=2,              # number of rows 
  ncol=2,              # number of columns 
  byrow = TRUE         # fill matrix by rows 
)        

A                      # print the matrix

b <- makeCacheMatrix(A)
cacheSolve(b)
cacheSolve(b)



