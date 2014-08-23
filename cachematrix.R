## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## Returns a list containing 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  


## cacheSolve: Checks if the inverse of the special "matrix" has been cached. 
## Calculates the inverse of the special "matrix" if it does not exist.
## Returns the inverse.
## 


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
} 