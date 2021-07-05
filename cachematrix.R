makeCacheMatrix<-function(x=matrix())
{
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInvr<-function(inverse){inv<<-inverse}
  getInvr<-function(){inv}
  list(set=set,get=get,setInvr=setInvr,getInvr=getInvr)
}

cacheSolve<-function(x,...)
{
  inv<-x$getInvr()
  if(!is.null(inv))
  {
    message("Retrieving cached data")
    return(inv)
  }
  mtrx<-x$get()
  inv<-solve(mtrx,...)
  x$setInvr(inv)
  inv
}
