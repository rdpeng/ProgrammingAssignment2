#title: Creating functions to make and retrieve inverse of matrix from cache.
#author  : Darshit Jethlia

## Write a short comment describing this function
#This function creates a Matrix and stores it in the cache.
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<- function(inv) m<<-inv
  getinverse<- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
# This function looks for results in the cache. If found, it returns the result from the cache, otherwise it evaluates the inverse of the matrix and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
  if(!is.na(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<- inv(data,...)
  x$setinverse(m)
  m
}
