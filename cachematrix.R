## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
##setting matrix
  set<-function(y){
    x<<-y
    i<<-NULL
}
##getting matrix
  get<-function()x
  
##Setting the inverse of matrix
  setInverse<-function(inverse) i<<-inverse
  
##getting the inverse of matrix
  getInverse<-function() i
  
##Making a list of properties
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  }
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
##Return a matrix that is inverse of "x"
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ##Getting matrix from object
  data<-x$get()
  
  ##Calculating the inverse using matrix multiplication
  i<-solve(data,...)
  
  ##Setting the inverse to the object
  x$setInverse(i)
  i
}


