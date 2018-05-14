## makeCacheMatrix creates a special matrix which is really a list containing a function to
## 1. set the value of the matrix
##2. get the value of the matrix
##3.set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setsolve<-function(solve)inv<<-solve
  getsolve<-function()inv
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## function computes the inverse of the matrix , in case not stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getsolve()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setsolve(inv)
}
