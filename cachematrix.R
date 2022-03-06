
##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than computing it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
##Your assignment is to write a pair of functions that cache the inverse of a 
##matrix.Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than computing it repeatedly 
##(there are also alternatives to matrix inversion that we will not discuss here). 
##Your assignment is to write a pair of functions that cache the inverse 
##of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
n<-NULL
set<-function(y) {
  x<<-y
  n<<-NULL
}
get<-function() x
setinverse<-function(inverse) n<<-inverse
getinverse<-function() n
list(set=set, get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n<-x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  mat<-x$get()
  n<-solve(mat, ...)
  x$setinverse(n)
  n
}
