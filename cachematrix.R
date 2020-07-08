## Put comments here that give an overall description of what your
## functions do
## the fuctions take in a matrix as input and cache it. the 2nd function later calculates the inverse of the matrix by using the solve() 
## function and then returns it

## the function is meant to cache the values of matrix and the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## the functions calculates and returns the inverse of a matrix by solve() function

cacheSolve <- function(x, ...) {
  inv<-x$geInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  inv
}
