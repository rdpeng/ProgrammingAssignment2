## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function makes inverse matrix as per the requirement of function on anywhere on workpad
## we need to set then get the matrix as given
## then we need to set and get the inverse of matrix as per given requirement 

makeCacheMatrix <- function(x = matrix()) {
 j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


##if we have stored inverse matrix on another environment and we required this function
## we can not revcall this function because it will time consuming and we can't afford it
## so here we are using lexical scooping advantage
## we store values in another environment the recall by cachesolve 
##function which will return inverse matrix as per the given matrix 


cacheSolve <- function(x, ...) {
	 j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
        ## Return a matrix that is the inverse of 'x'
}
