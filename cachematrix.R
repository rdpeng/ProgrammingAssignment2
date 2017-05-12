## Our function makeCacheMatrix  creates a matrix who can cache its inverse
##  Function cachesolve  is used to compute inverse of matrix created by the above function .If inverse has already been calculated  ( and matrix has not changed)then  cachesolve retrieves the same inverse  without computing. 
## we  assume that the matrix supplied is always invertible.
## This function creates a matrix that can cache its inverse. 
##It has many steps as we need to initialise this matrix 
## use set function to set the value of matrix 
## use get function to get the value of matrix 
## use setinverse to set the inverse of matrix ( although inverse also should be kept NULL initially )
## use getinverse to get the inverse of matrix
## use list in order to obtain the (getinverse , setinverse, get , set )  matrix values easily by using their names 
 makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set  =  function(y  ) 
  {
    
    x <<- y 
    i <<- NULL }
  get <- function() x
  setinverse <- function(solve) i <<- solve(x)
  getinverse <- function() i
  list( setinverse = setinverse, get = get, getinverse = getinverse, set = set )
 }
 ## Put comments here that give an overall description of what your
 ## functions do
 
 ## Write a short comment describing this function
 
 ##cachesolve function computes invese of matrix if it is NULL  
 ## If its not NULL then it retrieves the value if we are asking about the same matrix 
 ## This  function has steps 
 ## It assigns the value of inverse to a varible 
 ## Using IF loop to inspect value of inverse and return the same if not NULL
 ## If its NULL then use solve to compute inverse 
 ## Use setinverse to set computed inverse as inverse
 makeCacheMatrix <- function(x = matrix()) {
   
 }
 
 
 ## Write a short comment describing this function
 
 cacheSolve <- function(x, ...) {
   i <-  x$getinverse()
   if (!is.null(i)) {
     message("getting inverse")
     return(i) }
   
   data <- x$get()
   i <- solve(data)
   x$setinverse(i)
   
   ## Return a matrix that is the inverse of 'x'
   i
 }
