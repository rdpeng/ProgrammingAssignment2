## Put comments here that give an overall description of what your
## functions do
# R-program code that stores the values of matrix and caches its inverse

## The following function creates a special Matrix 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{
  flip<-NULL
  set <- function(y)
  {
    x<<-y
    flip<<-
  }NULL
  get<-function() x
  setinverse<-function(inverse)
    flip<<-inverse
  
  getinverse<-function() flip
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)

}

## This function computes the inverse of the special Matrix.
##If the inverse has already been calculated then
##it should retrieve the inverse from the cache


cacheSolve <- function(x, ...)
{
  flip<-x$getinverse()
  if(!is.null(flip))
  {
    message("Getting Cached Data")
    return(flip)
  }
  data<-x$get()
  flip<-inverse(data, ...)
  x$setinverse(flip)
  flip
}

