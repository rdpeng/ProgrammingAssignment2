## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix 
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The second function cacheSolve creates the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been created (and the matrix has not changed), then cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
