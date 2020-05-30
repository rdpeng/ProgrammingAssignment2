## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() i
  setInverse<-function(inverse) i<-solve
  getInverse<-function() i
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(i)
  
  }
  data<-x$get()
  i<-solve(data,...)
  x$setInverse(i)
  i
}
