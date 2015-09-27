## The function "makeCacheMatrix" will create a special matrix object that can cache
## the inverse of that matrix so that the second function "CacheSolve" can call 
## this cache and doesn't need to recalculate it if it already has been calculated
## from the previous function.
## (structure of solution according to example from course website "caching the 
## mean of a vector")

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function "cacheSolve" returns the inverse of the matrix. If the inverse 
## has already been computed it takes the results from the previous function and 
## hence doesn't need to calculate the inverse again. If the inverse hasn't been 
## calculated it computes the inverse.Thus it safes computational time.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inv)
  inv
}