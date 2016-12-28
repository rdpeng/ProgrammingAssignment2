## These funcitons allow a matrix to be cached and output the inverse.

## This function allows a Matrix to be Cached when it is created. 

makeCacheMatrix<-function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) x <<- inverse
  getinverse<-function() m
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function allows a cached matrix to be solved, outputting the inverse of the cached matrix. 

cacheinverse<-function(x, ...) {
  m<-x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-inverse(data, ...)
  x$setinverse(m)
  m
}