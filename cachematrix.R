## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Cache matrix function is created below

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)     ## setting the value of the matrix
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## getting the value of matrix
  setInverse <- function(Inverse) ## setting value of inverse
  { 
          inv <- Inverse
  } 
  getInverse <- function() ##getting value of inverse
  {
          inv
  }    
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function
## Cachesolve function computes the inverse of the matrix returned by Makecachematrix function.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) ## if inverse already calculated cachesolve will retrieve the return from cache
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
