## Two functions to cache the inverse of a matrix

## This one create a function containing special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  get<- function() x
  setInv<-function( solveMatrix) inverse<-solveMatrix
  getInv<- function() inverse
  list(set=set,get=get,setInv=setInv, getInv=getInv)
  
  
}


## This one calculates the cache of the inverse matrix and cumputes it

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
}
