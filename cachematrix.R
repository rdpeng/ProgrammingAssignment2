# makeCacheMatrix creates a special "matrix" object that can cache its inverse.  
# The "matrix" is  really a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix
 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  			 
  set <- function(y){		
  x <<- y				 
  m <<- NULL			
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# It first checks if the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via setinverse function.
# Assumption: the matrix is always invertible.


cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
