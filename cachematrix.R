# Mohan Liu

## cache the inverse of a matrix
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function()x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function()i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## 
# check if the inverse has been calculated
# if so, get the inverse from the cache and skips the computation
# otherwise calculate the inverse of the data
# and set the value of the mean in the cache via the setinverse function

cacheSolve<-function(x, ...){
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}