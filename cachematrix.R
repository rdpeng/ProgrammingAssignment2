## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  SetInverse<-function(inverse)i<<-inverse
  GetInverse<-function()i
  list(set=set,
       get=get,
       SetInverse=SetInverse,
       GetInverse=GetInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$GetInverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data<- x$get()
    i<-solve(data,...)
    x$SetInverse(i)
    i
}
