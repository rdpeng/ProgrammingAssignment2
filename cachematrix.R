library(MASS)
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x   #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x
  }
  list(set=set, get=get,
  setinv=setinv, 
  getinv=getinv)
}
cacheSolve<-function(x,...)#gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){
                    message("getting cached data!")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setmean(inv)
  inv
  }
