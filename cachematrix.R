##This function creates a special type of "matrix",actually a list, which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


##This is a function which can calculate the inverse of a square matrix. And if one has already been calculated,
##it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
 m<-x$getinverse()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
