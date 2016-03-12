## This R code helps to compute inverse of a matrix(If possible) in a optimized way by using caching mechanism.
## If the inverse of mateix is present in the cache then output is taken from cache else we are computing the inverse and storing in 
## a cache. Value in cache stays intact untill there is a change a input matrix

## The function makeCacheMatrix creates a special matrix which return a list with four functions. Using the functions we can
## set the value of input matrix,get the value of input matrix , set the value of inverse of the input matrix and get the value of
## inverse of input matrix from cache by using functions set,get,setinv,getinv.

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


## cacheSolve function computes inverse of a matrix if the value is not present in the cache and returns the inverse matrix.

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
