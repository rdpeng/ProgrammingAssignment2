## Coursera Week 3 Programming Assignment 2

## This function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(invertedMatrix = matrix()){
  cacheMatrix <- NULL
  
  get <- function() invertedMatrix
  
  set <- function(y)
  {
    invertedMatrix <<- y
    cacheMatrix <<- NULL
  }

  setinverse <- function(solve) cacheMatrix <<- solve
  getinverse <- function() cacheMatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- x$getinverse()
  
  if(!is.null(cacheMatrix))
  {
    message("getting cached data")
    return(cacheMatrix)
  }
  
  data <- x$get()
  
  #For this assignment, assume that the matrix supplied is always invertible.
  cacheMatrix <- solve(data, ...)
  
  x$setinverse(cacheMatrix)
  
  cacheMatrix
}
