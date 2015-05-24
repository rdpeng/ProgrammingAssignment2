makeCacheMatrix <-function(x=matrix())
{
  xinverse<-NULL
  set<-function(y)
  {
    x <<-y
    xinverse<-NULL
  }
  get<-function()
    setInv<-function(inv) xinverse<<-inv
  getInv<-function() xinverse
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


CacheSolve<-function(x,...){
  m <-x$getInv
  if (!is.null(m))
{
    message("Getting Cached data for inverse")
    return(m)
  }
data<-x$get
m<-solve(data)
x$setInv(m)
m

}
