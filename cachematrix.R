## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

# livrary(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           
  set <- function(y){
         x<<-y
         inv<<-NULL 
  }
  #function to get matrix x
  get <- function(){x}   
  setinv<-function(inverse)
    inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x       #this function to obtain inverse of the matrix
                    
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This is used to get the cache data

cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)) {
              message("getting cached data")
              return(inv)                 #return inverse value
  }
  data<-x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  #return a matrix that is the inverse of 'x'
}
