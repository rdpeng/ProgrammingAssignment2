## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function makes a matrix of size root of its input vector length and assigns that matrix a number of helper functions
#including most importantly, the inverse of its values if that inverse has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  x <- matrix(x, nrow = sqrt(length(x)), ncol = sqrt(length(x)))
  i <- NULL
  set <- function(y) {
    x <<- matrix(y, nrow = sqrt(length(y)), ncol = sqrt(length(y)))
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function takes a cacheMatrix and returns its inverse, taking the stored value if it has been calculated already 
#and informs the user that it has done so.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
