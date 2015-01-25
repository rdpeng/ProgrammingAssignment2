
#caching the inverse of a matrix with 
#the assumption that x is always invertible matrix
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function (x){
  #check the input  
  if(!is.matrix(x)){
    stop("Input should be a matrix")
    
    #if it is ok go ahead make function to set the matrix or get it    
  }else{
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) m<<- solve
    getinverse<-function() m
    list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
  }
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
#has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x, ...) {
  #check the input  
  m<-x$getinverse()
  if(!is.null(m)){
    message("Use the cached inverse.")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
  
}
