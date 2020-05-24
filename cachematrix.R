## Put comments here that give an overall description of what your
## functions do

# Two functions for cashing a matrix and then find its inversed one

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL # Will hold value of matrix inverse
  
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  
  get <- function() x #returns value of the matrix argument
  
  setInverse <- function(inverse) iv <<- inverse
  getInverse <- function() iv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getInverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setInverse(iv)
  iv
}
