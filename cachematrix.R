##'makeCacheMatrix()' & 'cacheSolve()' are a pair of functions that cache the inverse of a matrix

##'makeCacheMatrix' is a function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##takes argument 'x': a square invertible matrix
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##function to set the matrix
  
  get <- function() x
  ##function to get the matrix
  
  setinverse <- function(solve) inv <<- solve
  ##function to set the inverse of the matrix
  
  getinverse <- function() inv
  ##function to get the inverse of the matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##returns a list of the previous functions, which is used as the input to 'cacheSolve()'
}

##'cacheSolve()' is a function that computes the inverse of the special "matrix" 
##returned by 'makeCacheMatrix()'. If the inverse has already been calculated and 
##the matrix has not changed, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ##takes argument 'x': the output of 'makeCacheMatrix()', which in turn must have a matrix as its argument
  
  inv <- x$getinverse()
  ##uses the 'getinverse()' function listed in the output of 
  ##'cacheSolve()' to determine if the inverse has been calculated
  
  if(!is.null(inv)) {
    ##if the inverse has already been calculated
    
    message("getting cached data")
    ##return this message
    
    return(inv)
    ##then return the value of the inverse of the matrix
  }
  ##otherwise continue to the following steps
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  ##uses the 'setinverse()' function listed in the output of 
  ##'cacheSolve()' to set the value of the inverse in the cache
  
  inv
  ##then returns a matrix that is the inverse of 'x'
}