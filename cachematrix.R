## The first function, 'makeCacheMatrix', creates a special matrix object that 
## can cache its inverse and, the second function 'cacheSolve' computes the
## inverse of the matrix returned by 'makeCacheMatrix'

## Creates a special matrix object that can cache its inverse, which is 
## actually a list containing a function to: 
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  
 inverse<-NULL
 set<-function(y){
     x<<-y
     inverse<<-NULL
  }
  
  get<-function()x
  setinverse<-function(solve)inverse<<-solve
  getinverse<-function()inverse
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
 }
## Computes the inverse of the matrix returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$setinverse(inverse)
  inverse
  
        ## Return a matrix that is the inverse of 'x'
}
