## Put comments here that give an overall description of what your
## functions do

## create matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
  #set the matrix
  set=function(y)
  { 
    x<<-y
    inv<<-NULL
  }
  
  #get the matrix
  get=function() x
  
  #set the inverse
  setinv<-function(inverse) inv<<-inverse
  
  #get the inverse
  getinv <-function() inv
  
  #make the list
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x will be the output of makematrix
  
  inv<-x$getinv()
  
    if(!is.null(inv)){
       #inv is already cached
      message("cached data")
      return(inv)
    }
  
    mat.data<-x$get()
    inv=solve(mat.data,...)
    x$setinv(inv)
   return(inv)
        
}
