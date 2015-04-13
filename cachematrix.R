## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##    setmatrix will set the matrix to variable x
##    getmatrix will get the matrix value stored 
##    setinverse will cache the value of inverse 
##    getinverse will take the value of inverse stored if any.

makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        
        setmatrix <-function(y){
          x<<-y
          imatrix<<-NULL
        }
        
        getmatrix <-function() x
        
        setinverse <-function(inverse) imatrix<<- inverse
        
        getinverse <- function()  imatrix 
  
        list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
  
}



## Write a short comment describing this function
## the value of inverse is first stored to the variable imatrix
## if imatrix is not NUll ie there is a cache already available, print the message and return the 
## value. else compute the inverse and store the value as the cache for latter use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      imatrix<-x$getinverse
      if(!is.null(imatrix)){
        print("returning cached inverse of matrix")
        return(imatrix)
      } 
      
        data<-x$getmatrix()
        imatrix<- data^-1
        x$setinverse(imatrix)
        imatrix
        
      
}
