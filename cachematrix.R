## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly. 
# Below are two functions that are used to create a special object that stores 
# a matrix and caches its inverse


## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
# It is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## Write a short comment describing this function
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)){
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}
