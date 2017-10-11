## Put comments here that give an overall description of what your
## functions do
#if something is needed frequently, it is better to calculate values once and cache them than calculate every time you need to use these values 
# there are two functions at this file,makeCacheMatrix, cacheSolve
#makeCacheMatrix: to cache a matrix x 
#cacheSolve: to get the inverse of caches matrix
## Write a short comment describing this function
#This fuction caches a matrix to optimize using of resources.

makeCacheMatrix <- function(x = matrix()) 
  {
    inverse=NULL
    set<-function(y)
    {
      x<<-y
      inverse<<-NULL
    }
    get<- function()
    {
      x
    }
    setinverse<-function(inv)
    {
      inverse<<-inv
    }
    getinverse<-function()
    {
      inverse
    }
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

  }


## Write a short comment describing this function
# calling this function return an inverse of a cached matrix "done by the previous function"
cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinverse()
    if(!is.null(inverse))
    {
      message("Getting Cached Data")
      return(inverse)
    }
    data<-x$get()
    inverse<-solve(data,...)
    x$setinverse(inverse)
    inverse
  }
