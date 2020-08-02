## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

## the following function creates a special "matrix" object that can cache its inverse
MyCacheMatrix<-function(x=matrix()){
  t<- NULL
  set<-function(y){
    x<<-y
    t<<-NULL}
  get<-function()x
  setinverse<-function(inverse) 
    t<<-inverse
  getinverse<-function()t
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
  }


## Write a short comment describing this function


       
cacheMyInverse<-function(x,...) {t<-x$getinverse()
if (!is.null(t)){
        
        ##the following function will return a matrix that is the inverse of 'x' 
message("taking cached data") 
  return(t)
  }
data <- x$get()
t<- solve(data, ...)
x$setinverse(t)
t
}
