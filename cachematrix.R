## Programming Assignment 2
## Create a matrix then cache the inverse of that matrix

## This function creates a special matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     setx<-function(y){
          x<<-y
          m<<-NULL
     }
     getx <-function() x
     setinverse <-function() m<<-solve(x)
     getinverse <-function() m
     list(setx=setx, getx=getx, 
          setinverse=setinverse,
          getinverse=getinverse)
     
}


## This function calculates the inverse of the matrix created
## above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m<-x$getinverse()
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     data<-x$getx()
     m<-solve(data)
     x$setinverse()
     m
}
