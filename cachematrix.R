makecachematrix<- function(x=matrix()){
  a<-NULL
  set<- function(y){
    x<<- y
    a<<-NULL
  }
  get<-function() (x)
  setInverse<- function(inverse) (a<<-inverse)
  getInverse<-function() (a)
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
cachesolve<- function(x, ...){
  inv<- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
    
  }
  mat<- x$get()
  a<- solve(mat, ...)
  a
} 