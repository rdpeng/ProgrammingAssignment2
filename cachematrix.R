## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix - a function for creating a matrix that can make cache of its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<- function(inverse) inv
  getInverse<-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
#cacheSolve- a function for computing inverse matrix for the matrix from makeCacheMatrix function
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m<-x$get()
  inv<-solve(m, ...)
  x$setInverse(inv)
  inv

  }
