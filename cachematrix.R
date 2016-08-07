
## This program contains two functions: makeCacheMatrix and cacheSolve. The
## first returns a list of functions taking a matrix as input, and the 
## second function returns the inverse of 'x'


## "makeCacheMatrix" creates a list containing a function to set and get the 
## value of the matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     
     if (ncol(x)==nrow(x)) {         ##requirement to be square matrix
          inv <- NULL     
          set <- function (y=matrix()) {
               x <<- y
               inv <<- NULL
          }
     
          get <- function() x
          set_inverse <- function(inverse=matrix()) inv <<- inverse
          get_inverse <- function() inv
     
          list(set=set, get=get, 
               set_inverse=set_inverse, 
               get_inverse=get_inverse)
     } else{ print("Insert a square matrix of length (nxn)")} 
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$get_inverse()
      if (!is.null(inv)){
           message("getting cached data")
           return(inv)
      }
      
      data <- x$get()
      inv <- solve(data, ...)
      x$set_inverse(inv)
      inv
}
