## Functions to invert squared matrices

## MakeCacheMatrix create a list of 4 functions, 
## setmatrix, getmatrix, setinvert, getinvert 

makeCacheMatrix <- function(x = matrix()) {
  getmatrix<-function() x
  setmatrix<-function(y){
    x<<-y
    i<<-NULL
  }
  setinvert<-function(invert) i<<-invert
  getinvert<-function() i
  list(getmatrix=getmatrix,setmatrix=setmatrix,
       setinvert=setinvert,getinvert=getinvert)
}


## Cachesolve apply funtion "solve" to set matrix 

cacheSolve <- function(x, ...) {
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## Return stored invert matrix
  } 
  data<-x$getmatrix() 
  i<-solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setinvert(i) ## Store invert in cache
  i ## Return invert matrix
}
