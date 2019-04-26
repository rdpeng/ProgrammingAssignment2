## This the second peer-reviewed assignment;to test and try lexical scoping in R; 
## by Alimohammad Pourkhesalian
##
## the function takes in a square convertible matrix and stores it in a special matrix. 
## The outut of the function is a list of four functions
##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the inverse matrix
##4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
  
  
}

## The following function calculates the mean of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



