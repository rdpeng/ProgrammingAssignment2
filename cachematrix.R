## Second Programming Assignment 
## This  set of functions extends the concept of object "matrix" to "matrix and its inverse"
## which is cached alonside the original one.

## This function creates the makeCacheMatrix object that consists of a matrix
## and its inverse. The setinverse and getinverse operations on the new object 
## are also defined (set and get)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse
        )
}


## This function computes the inverse of matrix object of type makeCacheMatrix function
## if it is not already stored in cache. 
## This function extends the solve function to the new object makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinverse()
  if(!is.null(m)){
      message("getting cached inverse")
      return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
