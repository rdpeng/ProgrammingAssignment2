## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
##computing it repeatedly. So below function are able to cache potentially time consuming computation.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  get <- function() x
  imat <- NULL
  set <- function(y) {
    x <<- y
    imat <<- NULL
  }
  
  setinverse <- function(invrs) imat <<- invrs
  getinverse <- function() imat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets 
##the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  imat <- x$getinverse()
  if(!is.null(imat)){
    message("getting cached data")
    return(imat)
  }
  data<-x$get()
  imat<-solve(data,...)
  x$setinverse(imat)
  imat  ## Return a matrix that is the inverse of 'x'
}
