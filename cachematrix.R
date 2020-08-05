##MakeCacheMatrix is a function creating a special matrix object that can cache its inverse

## cacheSolve returns the inverse of the matrix created with the MakeCacheMatrix function


makeCacheMatrix <- function(x = matrix()) {
  dgf<-NULL
  set<-function(y){
    
  }
get<-function()x
setinverse<-function(inverse)dgf<<-inverse
getinverse<-function()dgf
list(set=set,get=get,
     setinverse=setinverse
     getinverse=getinverse)
}


## cacheSolve requires an argument that is returned by makeCacheMatrix()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
dgf<-x$getinverse()
if(!is.null(dgf)){
  message("getting cached data")
  return(dgf)
}
data<-x$get()
dgf<-inverse(data,...)
x$setinverse(dgf)
dgf
}
