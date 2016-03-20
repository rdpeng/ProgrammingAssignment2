##These functions were written for the Coursera R Programming Course Week 3 Assignment. 

## This function creates a list with methods which could set and get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     set<-function(y){
             x<<-y
             inv<<-NULL
     }
     get<-function()x
     setinverse<-function(inverse){inv<<-inverse}
     getinverse<-function()inv
     list(set=set, get=get, 
     setinverse=setinverse, 
     getinverse=getinverse)
}


## This function attempts to compute the inverse of the matrix returned by the above function.
##If the inverse has already been calculated, the cached value will be used.

cacheSolve <- function(x, ...) {
          inv<-x$getinverse()
          if(!is.null(inv)){
             message("getting cached data")
             return(inv)
}
          data<-x$get()
          inv<-solve(data,...)
          x$setinverse(inv)
          inv
}

