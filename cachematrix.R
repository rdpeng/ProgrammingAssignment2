## These functions will return the inverse of a matrix, either calculating it 
## or getting it from the stored data

## This function will save a calculated inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  y<-NULL
  get<-function() x
  setmatrix<-function(solve) y<<- solve
  getmatrix<-function() y
  list(get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

## This function will get the cached inverse matrix or calculate it

cacheSolve <- function(x=matrix(), ...) {
  y<-x$getmatrix()
  if(!is.null(y)){
    message("getting cached inverse matrix")
    y
  }else{
    message("calculating inverse matrix")
    matrix<-x$get()
    y<-solve(matrix, ...)
    x$setmatrix(y)
    y 
  }
  
}
