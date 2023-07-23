## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  jagoo<-NULL
  set<-function(y){
    x<<-y
    jagoo<<-NULL
  }
get<-function()x
setjagoo<-function(inverse)jagoo<<-inverse
getjagoo<-function(){
       inver<-gjagoo(x)
       inver%*%x}
   list(set=set,get=get,
        setjagoo=setjagoo,
        getjagoo=get(jagoo))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      jagoo<-x$getjagoo()
      if(!is.null(jagoo)){
        message("getting cached data")
        return(jagoo)## Return a matrix that is the inverse of 'x'
      }
      data<-x$get()
      jagoo<-solve(data,...)
      x$setjagoo(jagoo)
      jagoo
}
f<-makeCacheMatrix(matrix(1:8,2,4))
f$get()
