## [Put comments here that describe what your functions do]
## Put comments here that give an overall description of what your
## functions do

##write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x  #function to get matrix
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## This function will be used to get the inverse

cacheSolve <- function(x, ...) {  #gets cache data
## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){   #checks if the inverse is NULL
  message("getting cached data")  #return message
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)  #calculates the inverse
  x$setInverse(j)
  j
}
