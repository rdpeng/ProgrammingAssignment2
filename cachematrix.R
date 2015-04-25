## makeCacheMatrix makes a special cache matrix which supports th concept of caching by using <<- operator
## the cacheSolve function is used to evaluate the inverse of a matrix. 

## This function creates a special cache matrix which consist of four functions to set,get,get inverse and set inverse. A list is returned having these 4 functions as its elements. 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setinv<-function(inv) i<<-inv
  
  getinv<-function() i
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  

}


## This function returns the inverse of the special matrix passed as an argument. 
##It checks if the inverse is already calculated the cached inverse is returned. 
##Otherwise inverse is evaluated using solve function.
cacheSolve <- function(x, ...) {
      i<-x$getinv()
    
    if(!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
