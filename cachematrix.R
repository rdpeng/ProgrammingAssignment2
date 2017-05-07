## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function makes an arbitrary matrix
# It has cache cpabilities 
makeCacheMatrix <- function(x = matrix()) {
  # by default the invrs is a null function
  invrs <- NULL

  set <- function(y) {
    mtrx <<- y
    invrs <<- NULL
  }
  
  set(x)
  
  get <- function() mtrx
  
  setinverse <- function(inv) invrs <<- inv
  
  getinverse <- function() invrs
  
  list(set = set, get = get , setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv=x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  inverse <- solve(x$get())
  
  x$setinverse(inverse)
  inverse
}

