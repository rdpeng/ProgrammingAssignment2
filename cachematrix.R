##These functions were written for the Coursera R Programming Course Week 3 Assignment. 

## This function creates a matrix object which could catche its inverse.

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
             x<<-y
             m<<-NULL
     }
     get<-function()x
     setinverse<-function(inverse) m<<-inverse
     getinverse<-function()m
     list(set=set, get=get, 
     setinverse=setinverse, 
     getinverse=getinverse)
}


## This function attempts to compute the inverse of the matrix returned by the above function.
##If the inverse has already been calculated, the cached value will be used.

cacheSolve <- function(x, ...) {
          m<-x$getinverse()
          if(!is.null(m)){
             message("getting cached data")
             return(m)
}
          data<-x$get()
          m<-solve(data,...)
          x$setInverse(m)
          m
}

