## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}




## Write a short comment describing this function
##The following function calculates the inverse of the special “matrix” created with the above function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
}
