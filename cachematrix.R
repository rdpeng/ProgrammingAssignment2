## This file contains two functions, makeCacheMatrix() and cacheSolve()
## makeCacheMatrix(): creates an R object that stores a matrix and its inverse.
##cacheSolve(): requires an argument that is returned by makeVector() in order to retrieve the inverse 
##from the cached value that is stored in the makeCacheMatrix() object's environment.

## It builds a set of functions and returns the functions within a list to the parent environment, 
## the content are: set(), get(), setinv(), and getinv(). It also includes the two data objects, x and i (inverse of x).

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## This function populate or retrieve the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)## Retrieve and return matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i## Calculate and return a matrix that is the inverse of 'x'
        
}



