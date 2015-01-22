## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## fcacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix<-function(x){
  m<-NULL
  #set initial state of matrix and inverse
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #get matrix
  get<-function() x
  #set inverse matrix to other working environment
  setinverse<-function(solve) m<<-solve
  #get inverse matrix 
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve<-function(x){
  #get inverse, if inverser already exist, get the cached value
  m<-x$getinverse()
  if(!is.null(m)){
    return(m)
  }
  #calculate inverse matrix if it is not cached before
  data<-x$get()
  data
  m<-solve(data)
  x$setinverse(m)
  m
}
