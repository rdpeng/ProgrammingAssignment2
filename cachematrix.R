## Matrix Inversion

## this is a pair of functions that cache the inverse of a matrix.

## the first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  SetInv<-function(inverse){inv<<-inverse}
  getInv<-function(){inv}
  list(set=set,get=get,SetInv=SetInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
