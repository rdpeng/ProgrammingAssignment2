## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)
  {
    m<<-inverse
  }
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve<-function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("Get the inverse from the cache")
    return(m)
  }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    return(m)
}


