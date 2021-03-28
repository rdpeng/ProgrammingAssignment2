makeCacheMatrix<-function(a=matrix()) 
{
  #initializes two objects a and inve 
  inve<-NULL
  set<-function(b)
    {
    a<<-b
    inve<<-NULL
  }
  #define different functions inside makecacheMatrix
  get<-function()a
  setInverse<- function()inve<<- solve(a) # Solves the inverse of the matrix
  getInverse<- function()inve
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  #Creates a new object i.e a list and assigns each of the above functions as an element
}

cacheSolve<- function(a,...) #Returns a inverse matrix
{
  inve<-a$getInverse()
  if(!is.null(inve))
    {
    message("getting catched data")
    inve
    }
  mat<-a$get()
  inve<-solve(mat,...)
  a$setInverse(inve)
  inve
}
