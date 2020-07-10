makeCacheMatrix<- function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function(){x}
  setInverse<-function(invverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
