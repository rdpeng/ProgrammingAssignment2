## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rathar than compute it repeatedly
##The following pair of function, makeCacheMatrix and cacheSolve, are used to cache the inverse of a matrix.

## makeCachematrix creates a special matrix object that can cache its inverse

makeCacheMatrix<- function(x=matrix()){
  inv<-NULL
  set<-function(y)##set the value of the matrix {
    x<<-y
    inv<<-NULL
  }
  get<-function()x ##get the value of the matrix
  setinverse<-function(inverse) inv<<-inverse ##set value of inverse of the matrix
  getinverse<-function() inv ##get value of the inverse of the matrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

##cacheSolve computes the inverse of the special mztrix returned by makeCacheMatrix above.

cacheSolve<- function(x,...){
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv) 
  inv
}
