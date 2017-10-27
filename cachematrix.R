## Caching the inverse of a matrix
## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL

 ## Method to set the matrix
        set<- function(y){
                x<<-y
                m<<-NULL
}
        ##Method to get the matrix
        get<- function(){
                x
      }
##Method to set the inverse of the matrix
setInverse<- function(inverse){
i<<- inverse
}
        
## Method to get the inverse of the matrix
getInverse<- function(){
m
}
        
## Return a list of the methods
list(set=set,
     get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if( !is.null(m)){
                message("getting cached data")
                return(m)
}
## Get the matrix
  data<- x$get()
m<- solve(data)%% data
## Set the inverse
x$setInverse(m)
m
}
