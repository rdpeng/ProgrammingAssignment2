## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix description:
## In The makeCacheMatrix function we pass inversable matrix x and then we create functions
## "get" (to return matrix x as is ), "setMatrixInv" (to calculate and assign the inverse
## of the matrix) and "getMatrixInv" (to return cached inverse of the matrix. Function
## "set" is used to reset the m in global env to NULL and also assign or reset the original
## matrix x with new matrix "y" (This is when ever new matrix passed using x$set())
## All these four functions are passed as a parameter to list function which is returned 
## as it is the last statement
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setMatrixInv <- function(InvMat) m<<- InvMat
  getMatrixInv <- function() m
  
  list(set=set,get=get,
       setMatrixInv=setMatrixInv,
       getMatrixInv=getMatrixInv)
}


## Write a short comment describing this function
## cacheSolve description:
## To the cacheSolve function we pass the list which is returned by makeCacheMatrix 
## function. so the functions can be called using the x$ directly. Initially we get try to 
## get the matrix inverse from the cache by m<- x$getMatrixInv(). If m is not null means, 
## data exists in cache and will be return the data from cache. if m is null means no 
## data in cache exists. so will get the original matrix x by data <- x$get() and then 
## calculate the inverse by using solve function and store this calculated value using  
## x$setMatrixInv(m) into cache and also finally returns the inverse matrix.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInv()
  if(!is.null(m))
  {
    message("getting matrix inverse from Cache")
    return(m)
  }
  data <- x$get()
  m<-solve(data,...)
  x$setMatrixInv(m)
  m
}
