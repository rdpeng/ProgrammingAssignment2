## Put comments here that give an overall description of what your
## functions do

## Function checks if we have inv x calculated. 

makeCacheMatrix <- function(x = matrix()) 
  {
  invx <- NULL
  set <- function(y)
  {
    x <<- y
    invx <<- NULL
  }
  get <- function() {x}
  setinv <- function(solve) {invx <- solve}
  getinv <- function() {invx}
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
   invx <- x$getinv()
   if(!is.null(invx))
   {
     message ("getting cached data")
     return(inxv)
   }
   data <-x$get()
   invx <- solve(x, ...)
   x$setinv(invx)
   invx
        ## Return a matrix that is the inverse of 'x'
}
