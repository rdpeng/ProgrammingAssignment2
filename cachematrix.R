## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) { 
  inverseM <- NULL #init inverse as NULL
  set<-function(y) { 
    x<<-y
    inverseM<<-NULL
  }
  get<-function() {
    x
  }
  setinverseM<-function(inverse) {
    inverseM<<-inverse
  }
  getinverseM<-function() {
    inverted<-solve(x) 
    inverted  #obtain inverse of matrix
  }
  list(set=set,get=get,setinverseM=setinverseM,getinverseM=getinverseM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{ 
  inverseM<-x$getinverseM()
  if(!is.null(inverseM)) {    #check the inverse matrix is NULL or not
    message("getting cached data!")
    return(inverseM)
  }
  data<-x$get()
  inverseM<-solve(data,...) #getting inverse matrix value
  x$setinverseM(inverseM)
  inverseM
}
