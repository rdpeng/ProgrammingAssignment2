makeCacheMatrix=function(x = matrix()){
  invs=NULL
  set=function(y){
    x<<-y ## assign a value to an object in an environment 
    # different from the current environment. 
    invs=NULL
  }
  get=function() x
  setinvs=function(inverse) invs<<-inverse
  getinvs=function() invs
  list(set=set,get=get,setinvs=setinvs,getinvs=getinvs)
} 
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

cacheSolve=function(x,...){
  invs=x$getinvs()
  if(!is.null(invs)){ ## if the inverse has already been calculated
    message("getting cached data") # get it from the cache and skips the computation
    return(invs)
  }
  data=x$get() ## otherwise, calculates the inverse
  invs=solve(data,...)
  x$setinvs(invs) ## sets the value of the inverse in the cache via the setinvs function
  invs
}

####Test: 
# Test = matrix(c(2,0,0,2),2,2)
# x = makeCacheMatrix(Test)
# cacheSolve(x)
#       [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5

## Put comments here that give an overall description of what your
## functions do
# Function makeCacheMatrix() creates a "matrix" that can cache its inverse. 
#  Function cacheSolve() will calculate the inverse of the "matrix".
# In the function cacheSolve, it will check the inverse in the cache first. 
# If the inverse has aleady been calculated,it will return the stored value.
# If not, it will calculate the inverse and store the value in the cache for future.

## Write a short comment describing this function

# makeCacheMatrix <- function(x = matrix()) {

# }
# This function creates a "matrix" that can cache its inverse. It returns a list containing
# to set the matrix, get the matrix, set the inverse, and get the inverse. This list is used
# as the input to cacheSolve()

## Write a short comment describing this function

# cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
# }
# This function calculates the inverse. If it has already been calculated, it will be retrived from 
# the cache together with the message ("getting cached data"). Otherwise, it will calculate the 
# inverse and set the value in the cache. 