## Put comments here that give an overall description of what your
## functions do

####R Programming####
####Week 3 - Programming Assignment 2####
####
#### 
####This assignment is to write a pair of functions that cache the inverse of a matrix.
####The first function,  makeMatrix  creates a special "matrix" object , which can cache 
####the inverse of the matrix
##
####	1. Confirm that the matrix passed is a qualifying matrix. The error message 
####    will appear if the matrix does not contain an equal number of rows and cols
####	2. Set the value of the matrix
####	3. Get the value of the matrix
####	4. Set the value of the inverse of the matrix
####	5. Get the value of the inverse of the matrix
## 
##

makeCacheMatrix <- function(x = matrix()){
   inv <- NULL			
   
   if(!(nrow(x)==ncol(x))) stop('this is not a square matrix and thus not invertable')
   
   set <- function(y) {   
   x <<- y
   inv <<- NULL
   
   }
   
   get <- function() x
   setsolve <- function(solve) inv <<- solve
   getsolve <- function() inv
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
   
     
    }
  ## CacheSolve returns a matrix that is the inverse of 'x'
  ## 1. First, it verifies that the inverse exists and is not NULL.
  ## 2. If it exists in memory then it returns the inverse matrix
  ## 3. If it does not exist then it solves for the matrix and returns the matrix
  
   cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
     
      inv <- x$getsolve()
      
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
     }
      data <- x$get()
      inv <- solve(data, ...)
      x$setsolve(inv)
      inv
      
   }
   
   