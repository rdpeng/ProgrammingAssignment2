##makeCacheMatrix: This function creates a matrix object that can cache its inverse. 
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.
## functions do

## short comment describing this function
##This function creates a matrix object that can cache its
#set the value of the MATRIX
#get the value of the MATRIX
#set the value of the INVERSE
#get the value of the INVERSE

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##short comment describing this function
##The following function calculates the INVERSE of the MATRIX created with the above function. 
##However, it first checks to see if the INVERSE has already been calculated. 
##If so, it gets the INVERSE from the cache and skips the computation.
##Otherwise, it calculates the INVERSE of the MATRIX and sets the value of the INVERSE in the cache.

cacheSolve <- function(x, ...) {
      InM <- x$getinverse()
      if (!is.null(InM)) {
          message("getting the cached data")
          return(InM)
      }
  data <- x$get()
  InM <- solve(data, ...)
  x$setinverse(InM)
  InM
}
