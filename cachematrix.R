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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix hasn't changed), the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
         inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$SetInv(inv)
  inv## Return a matrix that is the inverse of 'x'
}
