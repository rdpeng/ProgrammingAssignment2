##AIM:
##   Write the following functions:

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.




## Put comments here that give an overall description of what your
## functions do
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


## I simply set the input x as a matrix
## and then set the solved value "sol" as a null
## then I changed every reference to "mean" to "inverse"

## Write a short comment describing this function
##makeCacheMatrix  function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   sol <- NULL
  set <- function(y){     ##set the value of the vector
  x <<- y
  sol <<- NULL
  }
  get <- function()x  ##get the value of the vector
  setInverse <- function(inverse) sol <<- inverse   ##set the value of the mean
  getInverse <- function() sol    ##get the value of the mean
  list(set = set, get = get, 
  setInverse = setInverse,   ## Produced with the list 
  getInverse = getInverse)

}


## Write a short comment describing this function

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix

##Same here, changed "mean" to "inverse" and "m" to "sol"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   sol <- x$getInverse()      ##checks If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
  if(!is.null(sol)){
  message("getting inversed matrix")
  return(sol)
  }
  mat <- x$get()  #gets the mat
  sol <- solve(mat,...) ##convert it to inverse.
  x$setInverse(sol)
  sol
}
