## Put comments here that give an overall description of what your
## functions do
# Functions modularize the operation flow of the program - providing 
# an easy to understand and easy to edit program.

## Write a short comment describing this function
# Creates an object to store a matrix and its inverse in its local environment - hence can be used like a cache
# if we call s <- makeCacheMatrix(a), then object 's' can store the matrix 'a' and also its inverse in its environment
# s$set - lets you reset the matrix in s
# s$get will give the matrix a
# s$setinv will set an inverse in a cache/its environment and s$getinv will output the inverse stored
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function(){
  x  
  }
  setinv <- function(t){
  i <<- t  
  }
  getinv <- function(){
  i  
  }
  list(set=set,get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# this function will check if the inverse is stored in object passed - x 
#(which is a CacheMatrix output from the previous function), and pull the inverse from the cache to output
# or calculate the inverse and update it in the cache and output the inverse
# b <- cacheSolve(s) - will give us the inverse of a - which was passed to makeCacheMatrix s
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message('getting cached data')
    return(i)
    }
  a <- x$get()  # get the matrix stored in x
  i <- x$setinv(solve(a))  # store its inverse in the environment of object x
  }
