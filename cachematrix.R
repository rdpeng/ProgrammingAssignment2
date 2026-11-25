## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ##initialize the inverse property
  ##set value of matrix
  set<-function(y){
    x<<-y ##new matrix assigned to x in parent environment
    inv<<-NULL ##reset cached inverse
  }
  ##get value of matrix
  get<-function()x
  ##set value of inverse
  setInverse<-function(inverse)inv<<-inverse
  ##get value of inverse
  getInverse<-function()inv
  ##list of the methods
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##check if inverse is cached
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ##if not cached, get the matrix, the inverse of the matrix, and cache it
  data<-x$get()
  inv<-solve(data,...)
  x$setInverse(inv)
  inv
}
