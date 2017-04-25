## Create two functions which perform caching of an inverted matrix.

## First function creates a matrix object x which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) a<<-inverse
  getinverse<-function() a
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Second function calculates the inverse of the matrix x returned
## by First function makeCacheMatrix.If the inverse has been computed already
## then the below function (cacheSolve) retrieves the inverse from the cache itself.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a<-x$getinverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data<-x$get()
  a<-solve(data,...)
  x$setinverse(a)
  a
}
