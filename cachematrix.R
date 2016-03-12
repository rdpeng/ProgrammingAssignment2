## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachem=NULL
  set=function(y){
    x<<-y
    cachem<<-NULL
  }
  
  get=function()x
  
  setinv=function(inv){
    cachem<<-inv
  }
  
  getinv=function()cachem
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()
  if(!is.null(inv)){
    message("Getting cached inverse matrix")
    return(inv)
  }
  m=x$get()
  inv=solve(m)
  x$setinv(inv)
  inv
}
