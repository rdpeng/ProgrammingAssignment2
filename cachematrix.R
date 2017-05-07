## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function makes an arbitrary matrix
# It has cache cpabilities 
makeCacheMatrix <- function(x = matrix()) {
  # by default the invrs is a null function
  invrs <- NULL

  # setting a matrix 
  set <- function(y) {
    mtrx <<- y
    invrs <<- NULL
  }
  
  # Ok, we need to do the set
  set(x)
  
  # a function to get the matrix
  get <- function() mtrx
  
  # this function sets the inverse
  setinverse <- function(inv) invrs <<- inv
  
  # printing the inverse 
  getinverse <- function() invrs
  
  # now the listing
  list(set = set, get = get , setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # setting the inverse of the matrix
  inv=x$getinverse()
  
  # checking wheter the inverse exists or not
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, do solve the matrix
  inverse <- solve(x$get())
  
  # now return it: first set the inverse
  x$setinverse(inverse)
  # now print the inverse
  inverse
}

