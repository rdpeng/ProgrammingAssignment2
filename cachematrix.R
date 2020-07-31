## Caching inverse of a matrix
## matrix inversion

## We first create a matrix

makeCacheMatrix <- function(x = matrix()) {
  a<- NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) a<<-inverse
  getInverse<- function()a
  list(set=set,get=get, setInverse= setInverse, getInverse=getInverse)
}


## Here we make a matrix that is inverse of x

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
  a<-x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat<-x$get()
  a<-solven(mat,...)
  x$setInverse(a)
}
