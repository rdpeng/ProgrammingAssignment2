## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to# set the value of the matrix 
# get the value of the matrix 
# set the value of inverse of the matrix 
# get the value of inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y)     
  {             
    x <<- y             
    inv <<- NULL       
  } 
  get <- function() x 
  setinverse <- function(inverse)
    inv <<- inverse getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}
## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function.    
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() 
  if(!is.null(inv)) 
  {  message("getting cached data.") 
    return(inv) 
  } 
  data <- x$get()
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
  
}
##sample run: 
## x <- matrix(data= c(4,5,6,7), nrow=2, ncol=2)
## m <-makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
##[1,]    4    6
##[2,]    5    7
## cacheSolve(m)
##      [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2 
## Retrieving from the cache in the second run
##cacheSolve(m)
##getting cached data.
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2