## The following two function is intended to create and set a matrix 
## and also to find and set (if not already set) the inverse of the given
## invertible matrix

## This function returns a list of functions that can set a matrix passed,
## get the matrix, set its inverse and get its inverse too.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This funtion will only canculate the inverse of the matrix passed if it hasn't 
## already been calculated. Else it will simply return the already calculated inverse.
## (We assume the matrix passed is invertible)

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
