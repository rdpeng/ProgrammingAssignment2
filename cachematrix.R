## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#I just took the example of the assigment and changed the mean for solved

makeCacheMatrix <- function(x=matrix()){
  m<- NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  get<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
  
}

cachesolve<- function(x,...){
  m<-x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
## Write a short comment describing this function


