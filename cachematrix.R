## [creates a special matrix of inverse whose eventual 
## goal is to cache the inverse of invertible matrix
## set the value of the matrix
## get the value of the matrix
## set the valye of the inverse
## get the value of the inverse]

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve)inv<<-solve
  getinv<-function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}

## this is the function to calculate the inverse 
## of the above created matrix
## it first checks if the inverse is available. if available
# it gets it from the cache via getinv function if not it
## calculates it and caches it via setinv function
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
