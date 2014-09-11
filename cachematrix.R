## Coursera R Language course: Programming Assignment 2
## functions to compute, cache, fetch the inverse of a matrix

## makeCacheMatrix creates an object with functions to set, get, setinverse and getinverse.
## The object is used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
 
m<-NULL
set <-function(y){
x<<-y
m<<-NULL
}
get <-function()x
setinverse<-function(inv) m<<-inv
getinverse<-function() m
list(set = set, get=get, 
setinverse=setinverse,
getinverse=getinverse)
}


## Computes the inverse of a matrix x. Uses object created by makeCacheMatrix. Funciton calls
## squarematrix to check if original data was good

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if (!is.null(m)) {  ## already exists so return cached answer
        message("getting cached data")
        return(m)
        }
        data <-x$get()  
         if(!squarematrix(data)){
  
 message("original argument not square numeric matrix ")
  
 return(NULL)
  }
        m<-solve(data)  ## call builtin function to solve data
        x$setinverse(m) ## save the answer for possible use later
        m
}

## helper function that checks if argument is a square numeric 2-dimensional matrix

squarematrix <- function(x) { 
   if (!is.numeric(x)) {
message("input not numeric")
return(FALSE)
}

   
   if (!is.matrix(x)) {
message("input not a matrix")  
return(FALSE)
   
}
   d<-dim(x)     ## check if square matrix
   if (length(d)!=2) {
message("input not 2-dimensional matrix")
return(FALSE)
   
}
   if(d[1]!=d[2]) {
message("input not square matrix") 
return(FALSE)
   
}
   return(TRUE)
   
}

