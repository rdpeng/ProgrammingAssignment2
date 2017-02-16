## The frist function, 'makeCacheMatrix', creates a special matrix object that 
## can cache its inverse and, the second function 'cacheSolve' computes the
## inverse of the matrix returned by 'makeCacheMatrix'
## functions do

## Creates a special matrix object that can cache its inverse, which is 
## actually a list containing a function to: 
##
## 1. set the value of the matrix
##
## 2. get the value of the matrix
##
## 3. set the value of the matrix's inverse
##
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  set<-function(y){
    
    x<<-y
    
    m<<-NULL
  }
  
  get<-function()x
  
  setinverse<-function(solve)m<<-solve
  
  getinverse<-function()m
  
  list(set=set,get=get,
       
       setinverse=setinverse,
       
       getinverse=getinverse)
}


## Computes the inverse of the matrix returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  
  m<-x$getinverse()
  
  if(!is.null(m)){
    
    message("getting cached data")
    
    return(m)
  }
  
  data<-x$get()
  
  m<-solve(data,...)
  
  x$setinverse(m)
  
  m        ## Return a matrix that is the inverse of 'x'
}
