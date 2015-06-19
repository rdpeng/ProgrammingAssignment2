## Function makeCacheMatrix() creates a "matrix" that can cache its inverse. 
## Function cacheSolve() will calculate the inverse of the "matrix".
## In the function cacheSolve, it will check the inverse in the cache first. 
## If the inverse has aleady been calculate,it will return the stored value.
## If not, it will calculate the inverse and store the value in the cache for future.

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
} ## return: a list containing functions to
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
  x$setinvs(invs) ## sets the value of the inverse in the cache via the setinv function
  invs
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
