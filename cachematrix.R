## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makecacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
   inv <- NULL
  set <- function(y)     ## setting the value of the matrix
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## getting the value of matrix
  setInverse <- function(Inverse) { inv <- Inverse} ## setting value of inverse
  getInverse <- function() {inv}    ##getting value of inverse
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
  if(!is.null(inv))     ## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
